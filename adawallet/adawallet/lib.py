import apsw
import json
import os
import re
import subprocess
import tarfile
import tempfile
from blockfrost import BlockFrostApi, ApiError
from collections import defaultdict
from colorama import Fore, Style
from pathlib import Path
from typing import Optional, Sequence, Protocol, List

# BlockFrost account details object
class BlockFrostAccountDetails(Protocol):
    withdrawable_amount: str

# BlockFrost UTXO unit and quantity object
class BlockFrostAmount(Protocol):
    unit: str
    quantity: str

# BlockFrost latest block object
class BlockFrostLatestBlock(Protocol):
    slot: int

# BlockFrost UTXO object
class BlockFrostUtxo(Protocol):
    tx_hash: str
    tx_index: int
    amount: Sequence[BlockFrostAmount]
    data_hash: Optional[str]
    inline_datum: Optional[str]
    reference_script_hash: Optional[str]

def input_mnemonic():
    data = input('Input 1st word or entire mnemonic: ')
    words = data.split(" ")
    if len(words) > 10:
        return words
    elif len(words) == 1:
        count=1
        while True:
            count += 1
            data = input(f"Word {count}: ")
            words.append(data)
            if data == "":
                return words


class AdaWallet:
    """A single address wallet library that supports mnemonics and hardware wallets"""

    def __init__(self, state_dir, debug):
        self.state_dir = Path(state_dir)
        self.accounts = {}
        self.db = None
        self.debug = debug
        self.wallet_initialized = False
        self.load_state()


    def initialize(self, testnet, rosetta_url=""):
        self.testnet = testnet
        self.rosetta_url = rosetta_url
        self.state_dir.mkdir(parents=True, exist_ok=True)
        db_exists = os.path.exists(self.state_dir / "data.sqlite")
        self.db = apsw.Connection(str((self.state_dir / "data.sqlite").resolve()))
        if not db_exists:
            self.initialize_db()
        self.load_state()


    def initialize_db(self):
        cursor = self.db.cursor()

        cursor.execute('''
          CREATE TABLE status (
            hw_wallet,
            root_key,
            testnet,
            rosetta_url
          )
        ''')

        cursor.execute('''
          CREATE TABLE utxo (
            txid                   TEXT     NOT NULL,
            tx_index               INTEGER  NOT NULL,
            address                TEXT     NOT NULL,
            amount                 INTEGER  NOT NULL,  -- 64 bit signed int for lovelace
            data_hash              TEXT,               -- hex or null
            inline_datum           TEXT,               -- hex cbor or null
            reference_script_hash  TEXT,               -- hex or null

            PRIMARY KEY (txid, tx_index)
          )
        ''')

        cursor.execute('''
          CREATE TABLE utxo_assets (
            txid                   TEXT     NOT NULL,
            tx_index               INTEGER  NOT NULL,
            policy_id              TEXT     NOT NULL,  -- 56 hex chars
            asset_name             TEXT     NOT NULL,  -- 0 - 64 hex chars
            quantity               INTEGER  NOT NULL,  -- 64 bit signed int

            PRIMARY KEY (txid, tx_index, policy_id, asset_name),
            FOREIGN KEY (txid, tx_index) REFERENCES utxo(txid, tx_index) ON DELETE CASCADE
          );
        ''')

        cursor.execute('''
          CREATE TABLE accounts (
            id,
            payment_vkey,
            payment_skey,
            stake_vkey,
            stake_skey,
            address,
            stake_address
          )
        ''')

        cursor.execute('''
          CREATE TABLE pparams (
            pp                     TEXT
          )
        ''')


    def load_state(self):
        if not os.path.exists(self.state_dir / "data.sqlite"):
            return
        if os.path.exists(self.state_dir / "data.sqlite") and not self.db:
            self.db: apsw.Connection = apsw.Connection(str((self.state_dir / "data.sqlite").resolve()))
        cursor = self.db.cursor()
        row = cursor.execute("SELECT * FROM status").fetchone()
        if row:
            self.wallet_initialized = True
            self.hardware_wallet = row[0]
            self.root_key = row[1]
            self.testnet = row[2]
            self.blockfrost = BlockFrostApi()
            if row[3]:
                # TODO: blockfrost URL stuff???
                pass
            if self.testnet:
                self.magic_args = ["--testnet-magic", "2"]
            else:
                self.magic_args = ["--mainnet"]
            rows = cursor.execute("SELECT * FROM accounts")
            for (index, payment_vkey, payment_skey, stake_vkey, stake_skey, address, stake_address) in rows:
                self.accounts[index] = {
                    "payment_vkey": payment_vkey,
                    "payment_skey": payment_skey,
                    "stake_vkey": stake_vkey,
                    "stake_skey": stake_skey,
                    "address": address,
                    "stake_address": stake_address
                }


    def wipe(self, prompt=True):
        if self.wallet_initialized and prompt:
            resp = input(f"Are you sure you want to wipe state directory {self.state_dir}? Type YES to confirm: ")
            if resp != 'YES':
                print('Aborting wipe!')
                exit(1)

        cli_args = [
            'rm',
            '-rf',
            self.state_dir,
        ]
        p = subprocess.run(cli_args, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error removing state directory")
        self.wallet_initialized = False


    def load_wallet_from_mnemonic(self, mnemonic):
        self.initialize_root_key(mnemonic)


    def initialize_hardware_wallet(self):
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO status VALUES(?,?,?,?)", (True, None, self.testnet, ""))
        self.hardware_wallet = True


    def initialize_read_only(self):
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO status VALUES(?,?,?,?)", (False, None, self.testnet, ""))


    def create_wallet_mnemonic(self):
        cli_args = [
            "cardano-address",
            "recovery-phrase",
            "generate",
        ]
        p = subprocess.run(cli_args, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error generating mnemonic")
        mnemonic = p.stdout.rstrip().split(" ")
        self.initialize_root_key(mnemonic)
        return mnemonic


    def initialize_root_key(self, mnemonic):
        cli_args = [
            "cardano-address",
            "key",
            "from-recovery-phrase",
            "Shelley",
        ]
        p = subprocess.run(cli_args, input=" ".join(mnemonic), capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error converting mnemonic to root key")
        root_key = p.stdout.rstrip()
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO status VALUES(?,?,?,?)", (False, root_key, self.testnet, ""))


    def import_accounts(self, start=None, end=None, accounts_file=None):
        if self.hardware_wallet:
            # HW wallet bulk import with one click
            self.import_accounts_hw(start, end, reload_state=False)
        elif start != None and end != None:
            for i in range(start, end + 1):
                self.import_account(i, reload_state=False)
        elif accounts_file:
            with open(accounts_file) as f:
                accounts = json.load(f)
            for account in accounts:
                self.import_account(account, reload_state=False)
        self.load_state()


    def import_accounts_hw(self, start, end, reload_state=True):
        account_indexes = range(start, end + 1)
        accounts = self.derive_account_keys_bulk_hw(account_indexes)
        for account_keys in accounts:
            self.write_account(account_keys)
        if reload_state:
            self.load_state()


    def import_account(self, account, reload_state=True):
        if type(account) == int:
            if account not in self.accounts:
                payment_vkey, payment_skey = self.derive_account_keys(account, "payment")
                stake_vkey, stake_skey = self.derive_account_keys(account, "stake")
                address = self.build_address(payment_vkey, stake_vkey)
                stake_address = self.build_stake_address(stake_vkey)
                account_keys = (account, payment_vkey, payment_skey, stake_vkey, stake_skey, address, stake_address)
                self.write_account(account_keys)
        elif type(account) == dict and account["index"] not in self.accounts:
            account_keys = (account["index"], account["payment_vkey"], account["payment_skey"], account["stake_vkey"], account["stake_skey"], account["address"], account["stake_address"])
            self.write_account(account_keys)
        if reload_state:
            self.load_state()


    def export_accounts(self, out_file):
        accounts = []
        for account_index, value in self.accounts.items():
            account = value.copy()
            account["index"] = account_index
            account["payment_skey"] = None
            account["stake_skey"] = None
            accounts.append(account)
        with open(out_file, 'w') as f:
            f.write(json.dumps(accounts))


    def write_account(self, account_keys):
        cursor = self.db.cursor()
        cursor.execute("INSERT INTO accounts VALUES(?,?,?,?,?,?,?)", account_keys)


    def build_address(self, payment_vkey, stake_vkey, era="latest"):
        with tempfile.NamedTemporaryFile("w+") as payment, tempfile.NamedTemporaryFile("w+") as stake:
            payment.write(payment_vkey)
            payment.flush()
            stake.write(stake_vkey)
            stake.flush()
            cli_args = [
                "cardano-cli",
                era,
                "address",
                "build",
                *self.magic_args,
                "--payment-verification-key-file",
                payment.name,
                "--stake-verification-key-file",
                stake.name
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                raise Exception(f"Unknown error building address")
        return p.stdout.rstrip()


    def build_payment_address(self, payment_vkey, era="latest"):
        with tempfile.NamedTemporaryFile("w+") as payment:
            payment.write(payment_vkey)
            payment.flush()
            cli_args = [
                "cardano-cli",
                era,
                "address",
                "build",
                *self.magic_args,
                "--payment-verification-key-file",
                payment.name,
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(" ".join("cli_args"))
                print(p.stderr)
                raise Exception(f"Unknown error building payment address")
        return p.stdout.rstrip()


    def build_stake_address(self, stake_vkey, era="latest"):
        with tempfile.NamedTemporaryFile("w+") as stake:
            stake.write(stake_vkey)
            stake.flush()
            cli_args = [
                "cardano-cli",
                era,
                "stake-address",
                "build",
                *self.magic_args,
                "--stake-verification-key-file",
                stake.name
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(" ".join("cli_args"))
                print(p.stderr)
                raise Exception(f"Unknown error building stake address")
        return p.stdout.rstrip()


    def derive_account_keys_bulk_hw(self, account_indexes):
        if self.hardware_wallet:
            accounts = []
            account_args = []
            key_files = []
            for i in account_indexes:
                payment_vkey = tempfile.NamedTemporaryFile("w+")
                payment_hws = tempfile.NamedTemporaryFile("w+")
                stake_vkey = tempfile.NamedTemporaryFile("w+")
                stake_hws = tempfile.NamedTemporaryFile("w+")
                account_args.extend(["--path", f"1852H/1815H/{i}H/0/0", "--hw-signing-file", payment_hws.name, "--verification-key-file", payment_vkey.name])
                account_args.extend(["--path", f"1852H/1815H/{i}H/2/0", "--hw-signing-file", stake_hws.name, "--verification-key-file", stake_vkey.name])
                key_files.append((i, payment_vkey, payment_hws, stake_vkey, stake_hws))

            cli_args = [
                "cardano-hw-cli",
                "address",
                "key-gen",
                *account_args
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                raise Exception(f"Unknown error extracting bulk accounts from hardware wallet")
            for account, payment_vkey, payment_hws, stake_vkey, stake_hws in key_files:
                if account not in self.accounts:
                    payment_hws_contents = payment_hws.read()
                    payment_vkey_contents = payment_vkey.read()
                    stake_hws_contents = stake_hws.read()
                    stake_vkey_contents = stake_vkey.read()
                    address = self.build_address(payment_vkey_contents, stake_vkey_contents)
                    stake_address = self.build_stake_address(stake_vkey_contents)
                    accounts.append((account, payment_vkey_contents, payment_hws_contents, stake_vkey_contents, stake_hws_contents, address, stake_address))
            return accounts


    def derive_account_keys(self, account, role):
        if role == "stake":
            role_index = 2
        elif role == "payment":
            role_index = 0
        else:
            raise Exception(f"Role {role} not supported!")
        if self.hardware_wallet:
            with tempfile.NamedTemporaryFile("w+") as vkey, tempfile.NamedTemporaryFile("w+") as hws:
                cli_args = [
                    "cardano-hw-cli",
                    "address",
                    "key-gen",
                    "--path",
                    f"1852H/1815H/{account}H/{role_index}/0",
                    "--hw-signing-file",
                    hws.name,
                    "--verification-key-file",
                    vkey.name
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0:
                    print(p.stderr)
                    raise Exception(f"Unknown error extracting account key {account}")
                hws_contents = hws.read()
                vkey_contents = vkey.read()
            return (vkey_contents, hws_contents)
        else:
            with tempfile.NamedTemporaryFile("w+") as vkeyx, tempfile.NamedTemporaryFile("w+") as vkey, tempfile.NamedTemporaryFile("w+") as skey:
                cli_args = [
                    "cardano-address",
                    "key",
                    "child",
                    f"1852H/1815H/{account}H/{role_index}/0",
                ]
                p = subprocess.run(cli_args, input=self.root_key, capture_output=True, text=True)
                if p.returncode != 0:
                    print(p.stderr)
                    raise Exception(f"Unknown error deriving child account key for {account}")
                child_skey = p.stdout.rstrip()
                cli_args = [
                    "cardano-cli",
                    "key",
                    "convert-cardano-address-key",
                    f"--shelley-{role}-key",
                    "--signing-key-file",
                    "/dev/stdin",
                    "--out-file",
                    skey.name
                ]
                p = subprocess.run(cli_args, input=child_skey, capture_output=True, text=True)
                if p.returncode != 0:
                    print(p.stderr)
                    raise Exception(f"Unknown error converting child key to CLI format for {account}")

                cli_args = [
                    "cardano-cli",
                    "key",
                    "verification-key",
                    "--signing-key-file",
                    skey.name,
                    "--verification-key-file",
                    vkeyx.name
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0:
                    print(p.stderr)
                    raise Exception(f"Unknown error converting signing key to verification extended key for {account}")

                cli_args = [
                    "cardano-cli",
                    "key",
                    "non-extended-key",
                    "--extended-verification-key-file",
                    vkeyx.name,
                    "--verification-key-file",
                    vkey.name
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0:
                    print(p.stderr)
                    raise Exception(f"Unknown error converting verification extended key to verification key for {account}")
                skey_contents = skey.read()
                vkey_contents = vkey.read()
            return (vkey_contents, skey_contents)


    def sign_msg(self, account, msg_file, out_file, stake=False, hashed=False, era="latest"):
        if account not in self.accounts:
            self.import_account(account)

        signing_args = []
        if hashed:
            signing_args.extend(["--hashed"])

        if self.hardware_wallet:
            with (
                tempfile.NamedTemporaryFile("w+") as payment_hws,
                tempfile.NamedTemporaryFile("w+") as stake_hws,
                open(msg_file, "rb") as msg_file_rb,
            ):
                msg_hex = msg_file_rb.read().hex()

                # Messages are expected to be signed with only one of a payment or stake key
                if stake:
                    stake_hws.write(self.accounts[account]["stake_skey"])
                    stake_hws.flush()
                    secret_key_hws = stake_hws.name
                    address = self.accounts[account]["stake_address"]
                else:
                    payment_hws.write(self.accounts[account]["payment_skey"])
                    payment_hws.flush()
                    secret_key_hws = payment_hws.name
                    address = self.build_payment_address(self.accounts[account]["payment_vkey"])

                cli_args = [
                    "cardano-hw-cli",
                    "message",
                    "sign",
                    "--message-hex",
                    msg_hex,
                    "--signing-path-hwsfile",
                    secret_key_hws,
                    "--address",
                    address,
                    "--address-hwsfile",
                    secret_key_hws,
                    "--out-file",
                    out_file,
                    *signing_args
                ]

                if self.debug:
                    print(f"def sign_msg: {" ".join(cli_args)}")

                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    print(p.stderr)
                    raise Exception(f"Unknown error signing message with account {account}")
            return
        elif self.accounts[account]["payment_skey"]:
            with tempfile.NamedTemporaryFile("w+") as payment_skey, tempfile.NamedTemporaryFile("w+") as stake_skey:
                signing_args = []

                # Messages are expected to be signed with only one of a payment or stake key
                if stake:
                    stake_skey.write(self.accounts[account]["stake_skey"])
                    stake_skey.flush()
                    secret_key = stake_skey.name
                    address = self.accounts[account]["stake_address"]
                else:
                    payment_skey.write(self.accounts[account]["payment_skey"])
                    payment_skey.flush()
                    secret_key = payment_skey.name
                    address = self.build_payment_address(self.accounts[account]["payment_vkey"])

                cli_args = [
                    "cardano-signer",
                    "sign",
                    "--cip8",
                    "--data-file",
                    msg_file,
                    "--secret-key",
                    secret_key,
                    "--address",
                    address,
                    "--json-extended",
                    "--include-maps",
                    "--out-file",
                    out_file,
                    *self.magic_args,
                    *signing_args
                ]

                if self.debug:
                    print(f"def sign_msg: {" ".join(cli_args)}")

                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(p.stderr)
                    raise Exception(f"Unknown error signing message with account {account}")
            return
        raise Exception(f"No signing key available for account {account}")


    def sign_tx(self, account, tx_body, out_file, stake=False, era="latest"):
        if account not in self.accounts:
            self.import_account(account)

        if self.hardware_wallet:
            with (
                tempfile.NamedTemporaryFile("w+") as payment_hws,
                tempfile.NamedTemporaryFile("w+") as stake_hws,
                tempfile.NamedTemporaryFile("w+") as tx_body_formatted,
                tempfile.NamedTemporaryFile("w+") as witness1,
                tempfile.NamedTemporaryFile("w+") as witness2
            ):
                signing_args = []
                assembly_args = []
                payment_hws.write(self.accounts[account]["payment_skey"])
                payment_hws.flush()
                signing_args.extend(["--hw-signing-file", payment_hws.name])

                cli_args = [
                    "cardano-hw-cli",
                    "transaction",
                    "transform",
                    "--tx-file",
                    tx_body,
                    "--out-file",
                    tx_body_formatted.name
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(tx_body_formatted.name):
                    print(" ".join(cli_args))
                    print(p.stderr)
                    raise Exception(f"Unknown error signing transaction with account {account}")
                if stake:
                    stake_hws.write(self.accounts[account]["stake_skey"])
                    stake_hws.flush()
                    signing_args.extend([
                        "--hw-signing-file", stake_hws.name,
                        "--out-file", witness2.name
                    ])
                    assembly_args.extend(["--witness-file", witness2.name])
                cli_args = [
                    "cardano-hw-cli",
                    "transaction",
                    "witness",
                    *self.magic_args,
                    "--tx-file",
                    tx_body_formatted.name,
                    "--out-file",
                    witness1.name,
                    *signing_args
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0:
                    print(" ".join(cli_args))
                    print(p.stderr)
                    raise Exception(f"Unknown error signing transaction with account {account}")

                cli_args = [
                    "cardano-cli",
                    era,
                    "transaction",
                    "assemble",
                    "--tx-body-file",
                    tx_body,
                    "--witness-file",
                    witness1.name,
                    "--out-file",
                    out_file,
                    *assembly_args
                ]

                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    print(p.stderr)
                    raise Exception(f"Unknown error assembling transaction with account {account}")
            return
        elif self.accounts[account]["payment_skey"]:
            with tempfile.NamedTemporaryFile("w+") as payment_skey, tempfile.NamedTemporaryFile("w+") as stake_skey:
                signing_args = []
                payment_skey.write(self.accounts[account]["payment_skey"])
                payment_skey.flush()

                signing_args.extend(["--signing-key-file", payment_skey.name])

                if stake:
                    stake_skey.write(self.accounts[account]["stake_skey"])
                    stake_skey.flush()
                    signing_args.extend(["--signing-key-file", stake_skey.name])

                cli_args = [
                    "cardano-cli",
                    era,
                    "transaction",
                    "sign",
                    "--tx-body-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]

                if self.debug:
                    print(f"def sign_tx: {" ".join(cli_args)}")

                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(p.stderr)
                    raise Exception(f"Unknown error signing transaction with account {account}")
            return
        raise Exception(f"No signing key available for account {account}")


    def bulk_witness_tx(self, tx_archive, out_file, role):
        with tarfile.open(name=out_file, mode='w:gz') as tarout, tarfile.open(name=tx_archive, mode='r:gz') as tarin:
                for member in tarin.getmembers():
                    with tempfile.NamedTemporaryFile("wb+") as tx, tempfile.NamedTemporaryFile("w+") as witness:
                        tx_buf = tarin.extractfile(member)
                        if tx_buf is None:
                            raise RuntimeError(f"Cannot extract {member.name} from the input TGZ file; do you have unusual permissions on this file?")
                        tx_contents = tx_buf.read()
                        tx.write(tx_contents)
                        tx.flush()
                        account = int(member.name.split(".")[0])
                        txwitness_name = f"{account}.txwitness-{role}"
                        self.witness_tx(account, tx.name, witness.name, role)
                        tarout.add(witness.name, txwitness_name)

    def bulk_sign_tx(self, tx_archive, out_file, stake=False):
        with tarfile.open(name=out_file, mode='w:gz') as tarout, tarfile.open(name=tx_archive, mode='r:gz') as tarin:
                for member in tarin.getmembers():
                    with tempfile.NamedTemporaryFile("wb+") as tx, tempfile.NamedTemporaryFile("w+") as sign:
                        tx_buf = tarin.extractfile(member)
                        if tx_buf is None:
                            raise RuntimeError(f"Cannot extract {member.name} from the input TGZ file; do you have unusual permissions on this file?")
                        tx_contents = tx_buf.read()
                        tx.write(tx_contents)
                        tx.flush()
                        account = int(member.name.split(".")[0])
                        txsigned_name = f"{account}.txsigned"
                        self.sign_tx(account, tx.name, sign.name, stake=stake)
                        tarout.add(sign.name, txsigned_name)


    def witness_tx(self, account, tx_body, out_file, role, era="latest"):
        if account not in self.accounts:
            self.import_account(account)

        if self.hardware_wallet:
            with tempfile.NamedTemporaryFile("w+") as hws:
                signing_args = []
                hws.write(self.accounts[account][f"{role}_skey"])
                hws.flush()
                signing_args.extend(["--hw-signing-file", hws.name])

                cli_args = [
                    "cardano-hw-cli",
                    "transaction",
                    "witness",
                    *self.magic_args,
                    "--tx-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    print(p.stderr)
                    raise Exception(f"Unknown error witnessing transaction with account {account}")
            return
        elif self.accounts[account][f"{role}_skey"]:
            with tempfile.NamedTemporaryFile("w+") as skey:
                signing_args = []
                skey.write(self.accounts[account][f"{role}_skey"])
                skey.flush()
                signing_args.extend(["--signing-key-file", skey.name])

                cli_args = [
                    "cardano-cli",
                    era,
                    "transaction",
                    "witness",
                    "--tx-body-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]

                if self.debug:
                    print(f"def witness_tx: {" ".join(cli_args)}")

                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(p.stderr)
                    raise Exception(f"Unknown error witnessing transaction with account {account}")
            return
        raise Exception(f"No signing key available for account {account} with role {role}")


    def clear_utxo_tables(self):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM utxo")
        cursor.execute("DELETE FROM utxo_assets")


    def update_utxos_for_accounts(self, filter_min, reload_state=False):
        self.clear_utxo_tables()
        cursor = self.db.cursor()

        total_utxo = 0
        total_utxo_count_nt = 0
        total_utxo_count_filter_min = 0

        for _, details in self.accounts.items():
            utxos, utxo_assets, utxo_count_nt, utxo_count_filter_min = self.get_utxos_for_address(details["address"], filter_min)

            for utxo in utxos:
                cursor.execute("INSERT INTO utxo VALUES(?,?,?,?,?,?,?)", utxo)

            for utxo_asset in utxo_assets:
                cursor.execute("INSERT INTO utxo_assets VALUES(?,?,?,?,?)", utxo_asset)

            total_utxo = total_utxo + len(utxos)
            total_utxo_count_nt = total_utxo_count_nt + utxo_count_nt
            total_utxo_count_filter_min = total_utxo_count_filter_min + utxo_count_filter_min

        print(f"Total UTXO imported: {total_utxo}; Total native token containing: {total_utxo_count_nt}; Total min filtered: {total_utxo_count_filter_min}")

        if reload_state:
            self.load_state()


    def import_protocol_parameters(self, pparams_json , reload_state=False):
        cursor = self.db.cursor()
        cursor.execute("DELETE FROM pparams")

        with open(pparams_json, 'r') as f:
            pparams = json.load(f)

        cursor.execute("INSERT INTO pparams VALUES(?)", (json.dumps(pparams),))

        if reload_state:
            self.load_state()


    def import_utxos_for_accounts(self, utxo_json, reload_state=False):
        self.clear_utxo_tables()

        with open(utxo_json, 'r') as f:
            utxos = json.load(f)

        cursor = self.db.cursor()
        for utxo_attrs in utxos:
            utxo = (utxo_attrs['txid'], utxo_attrs['tx_index'], utxo_attrs['address'], utxo_attrs['amount'])
            cursor.execute("INSERT INTO utxo VALUES(?,?,?,?)", utxo)

        if reload_state:
            self.load_state()


    def export_utxos_for_accounts(self, utxo_json):
        cursor = self.db.cursor()
        utxo_entries = []

        query = '''
          WITH

          -- UTXO for requested address.
          address_utxos AS (SELECT * FROM utxo WHERE address=?),

          -- Create one amount item with unit and quantity per lovelace or native token.
          amount_items AS (
            -- A lovelace row is always present
            SELECT
              au.txid,
              au.tx_index,
              json_object(
                'unit', 'lovelace',
                'quantity', CAST(au.amount AS TEXT)
              ) AS item,
              0 AS ord, NULL AS pol, NULL AS an
            FROM address_utxos au

            -- Ord, pol, an above and below are used for union sorting below.
            UNION ALL

            -- Native assets present
            SELECT
              ua.txid,
              ua.tx_index,
              json_object(
                'unit', ua.policy_id || ua.asset_name,
                'quantity', CAST(ua.quantity AS TEXT)
              ) AS item,
              1 AS ord, ua.policy_id AS pol, ua.asset_name AS an
            FROM utxo_assets ua
            JOIN address_utxos au
              ON au.txid = ua.txid AND au.tx_index = ua.tx_index
          ),

          -- Build the ordered amount array per (txid, tx_index)
          amount_json AS (
            SELECT
              ai.txid,
              ai.tx_index,
              (
                SELECT json_group_array(json(item))
                FROM amount_items ai2
                WHERE ai2.txid = ai.txid AND ai2.tx_index = ai.tx_index
                ORDER BY ai2.ord, ai2.pol, ai2.an
              ) AS amount_array
            FROM amount_items ai
            GROUP BY ai.txid, ai.tx_index
          ),

          -- Final UTxO objects
          utxo_objs AS (
            SELECT json_object(
              'txid', au.txid,
              'tx_index', au.tx_index,
              'address', au.address,
              'amount', json(aj.amount_array),
              'data_hash', au.data_hash,
              'inline_datum', au.inline_datum,
              'reference_script_hash', au.reference_script_hash
            ) AS obj
            FROM address_utxos au
            JOIN amount_json aj
              ON aj.txid = au.txid AND aj.tx_index = au.tx_index
            ORDER BY au.txid, au.tx_index
          )

          -- One JSON array of UTXO
          SELECT json_group_array(json(obj)) AS utxos_json
          FROM utxo_objs;
        '''

        for account, details in self.accounts.items():
            address = details["address"]
            row = cursor.execute(query, (address,)).fetchone()
            if row and row[0]:
                utxo = json.loads(row[0])
                print(f"Account {account} address {address} had {len(utxo)} UTXO exported")
                utxo_entries.extend(utxo)
            else:
                print(f"Account {account} address {address} had 0 UTXO exported")
        print(f"Total UTXO exported: {len(utxo_entries)}")

        with open(utxo_json, 'w') as f:
            f.write(json.dumps(utxo_entries, indent=2))


    def get_rewards_for_stake_address(self, stake_address):
        try:
            account_details: BlockFrostAccountDetails = self.blockfrost.accounts(stake_address=stake_address)
            return { stake_address: int(account_details.withdrawable_amount) }
        except ApiError as e:
            if e.status_code == 404:
                print(f"blockfrost: Stake address {stake_address} has no rewards information reported; returning 0")
                return { stake_address: 0 }
            else:
                print(f"Error obtaining rewards for stake address {stake_address}:")
                print(e)
                exit(1)


    def get_block(self, blockid=None):
        try:
            if blockid:
                return self.blockfrost.block(hash_or_number=blockid)
            return self.blockfrost.block_latest()
        except ApiError as e:
            print(e)
            exit(1)


    def get_slot_tip(self):
        block: BlockFrostLatestBlock = self.get_block()
        return block.slot


    def fetch_utxos_address(self, address, multiasset=False):
        scriptUtxos = []
        utxos = []
        utxo_assets = []
        cursor = self.db.cursor()

        # Set the lovelace table utxo query according to whether native tokens should be included.
        if multiasset:
            rows = cursor.execute("SELECT * FROM utxo WHERE address=?", (address,))
        else:
            # When NTs aren't desired, we need to filter lovelace that is
            # associated with NT containing UTXO from the utxo table.
            rows = cursor.execute('''
              SELECT * FROM utxo
                WHERE utxo.address=?
                  AND NOT EXISTS (
                    SELECT 1 FROM utxo_assets
                      WHERE utxo_assets.txid = utxo.txid
                        AND utxo_assets.tx_index = utxo.tx_index
                  )
            ''', (address,))

        # Process the query results into a list of tuples contains lovelace UTXO for the address.
        for row in rows:
            txid = row[0]
            index = row[1]
            amount = int(row[3])
            data_hash = row[4]
            inline_datum = row[5]
            reference_script_hash = row[6]

            # Only use UTXO that aren't "special", ie: data_hash, inline_datum
            # and reference_script_hashes may require or risk any of failing
            # validation, consuming a reference script, breaking reference
            # NFTs.
            if data_hash is None and inline_datum is None and reference_script_hash is None:
                utxos.append((txid, index, amount, data_hash, inline_datum, reference_script_hash))

            # If we actually run into a UTXO like this in adawallet, log it and
            # also prepare to remove any corresponding native token assets.
            else:
                scriptUtxos.append((txid, index))

        # Now generate the native token UTXO asset list for the address if they were requested.
        if multiasset:
            # Select UTXO native token assets which are held under the requested address.
            rows = cursor.execute('''
              SELECT utxo_assets.* FROM utxo_assets
                JOIN utxo ON utxo_assets.txid = utxo.txid
                  AND utxo_assets.tx_index = utxo.tx_index
                WHERE utxo.address=?
            ''', (address,))

            for row in rows:
                txid = row[0]
                index = row[1]
                policy_id = row[2]
                asset_name = row[3]
                quantity = row[4]

                # Don't include "special" UTXO which may be unintended and present risk.
                if (txid, index) not in scriptUtxos:
                    utxo_assets.append((txid, index, policy_id, asset_name, quantity))

        # Advise the user that some probably unexpected UTXO were considered for spending.
        for txid, index in scriptUtxos:
            print(f"Address {address} has UTXO {txid}#{index} which contains a data hash, datum or reference script, skipping...")

        return (utxos, utxo_assets)


    def stake_registration_tx(self, account, out_file, fee, ttl=None, sign=False, deposit=2000000, era="latest"):
        with tempfile.NamedTemporaryFile("w+") as stake_registration_certificate, tempfile.NamedTemporaryFile("w+") as vkey:
            vkey.write(self.accounts[account]["stake_vkey"])
            vkey.flush()
            cli_args = [
                "cardano-cli",
                era,
                "stake-address",
                "registration-certificate",
                "--stake-verification-key-file",
                vkey.name,
                "--key-reg-deposit-amt",
                str(deposit),
                "--out-file",
                stake_registration_certificate.name
            ]

            if self.debug:
                print(f"def stake_registration_tx: {" ".join(cli_args)}")

            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                raise Exception(f"Unknown error generating stake registration certificate for account {account}")
            return self.build_tx(account, out_file, fee, certificates=[stake_registration_certificate.name], ttl=ttl, sign=sign, deposit=deposit, stake=True)


    def bulk_stake_registration_tx(self, out_file, fee, ttl=None, deposit=2000000, sign=False):
        if sign:
            suffix="txsigned"
        else:
            suffix="txbody"

        sum_result = (0, 0, 0, 0)

        with tarfile.open(name=out_file, mode='w:gz') as tar:
            for account, _ in self.accounts.items():
                with tempfile.NamedTemporaryFile("w+") as tx:
                    result = self.stake_registration_tx(int(account), tx.name, fee, ttl, sign, deposit)
                    if result != (0, 0, 0, 0):
                        tar.add(tx.name, f"{account}.{suffix}")
                sum_result = (sum_result[0] + result[0], sum_result[1] + result[1], sum_result[2] + result[2], sum_result[3] + result[3])
        return sum_result


    def bulk_delegate_pool_tx(self, delegations_file, out_file, fee, ttl=None, sign=False):
        if sign:
            suffix="txsigned"
        else:
            suffix="txbody"

        sum_result = (0, 0, 0, 0)

        with open(delegations_file) as f:
            delegations = json.load(f)
        with open(out_file, "wb") as f2:
            with tarfile.open(fileobj=f2, mode='w:gz') as tar:
                for account, pool_id in delegations.items():
                    with tempfile.NamedTemporaryFile("w+") as tx:
                        result = self.delegate_pool_tx(int(account), pool_id, tx.name, fee, ttl, sign)
                        if result != (0, 0, 0, 0):
                            tar.add(tx.name, f"{account}.{suffix}")
                    sum_result = (sum_result[0] + result[0], sum_result[1] + result[1], sum_result[2] + result[2], sum_result[3] + result[3])
        return sum_result


    def bulk_drain_tx(self, send_addr, out_file, fee, multiasset=False, rewards=False, ttl=None, sign=False):
        if sign:
            suffix="txsigned"
        else:
            suffix="txbody"

        sum_result = (0, 0, 0, 0)

        with open(out_file, "wb") as f2:
            with tarfile.open(fileobj=f2, mode='w:gz') as tar:
                for account in self.accounts:
                    with tempfile.NamedTemporaryFile("w+") as tx:
                        result = self.drain_tx(int(account), send_addr, tx.name, fee, multiasset, rewards, ttl, sign)
                        if result != (0, 0, 0, 0):
                            tar.add(tx.name, f"{account}.{suffix}")
                    sum_result = (sum_result[0] + result[0], sum_result[1] + result[1], sum_result[2] + result[2], sum_result[3] + result[3])
        return sum_result


    def migrate_wallet(self, accounts_file, out_file, fee, multiasset=False, rewards=False, ttl=None, sign=False):
        if sign:
            suffix="txsigned"
        else:
            suffix="txbody"
        with open(accounts_file) as f:
            migrate_accounts = json.load(f)
        sum_result = (0, 0, 0, 0)

        with open(out_file, "wb") as f:
            with tarfile.open(fileobj=f, mode='w:gz') as tar:
                for account, _ in self.accounts.items():
                    with tempfile.NamedTemporaryFile("w+") as tx:
                        output_address = None

                        for migrate_details in migrate_accounts:
                            if int(account) == int(migrate_details["index"]):
                                output_address = migrate_details["address"]

                        if not output_address:
                            raise Exception("Error! accounts file must contain an address for every existing account to migrate")

                        result = self.drain_tx(int(account), output_address, tx.name, fee, multiasset, rewards, ttl, sign)
                        if result != (0, 0, 0, 0):
                            tar.add(tx.name, f"{account}.{suffix}")

                    sum_result = (sum_result[0] + result[0], sum_result[1] + result[1], sum_result[2] + result[2], sum_result[3] + result[3])
        return sum_result


    def delegate_pool_tx(self, account, pool_id, out_file, fee, ttl=None, sign=False, era="latest"):
        with tempfile.NamedTemporaryFile("w+") as delegation_certificate, tempfile.NamedTemporaryFile("w+") as vkey:
            vkey.write(self.accounts[account]["stake_vkey"])
            vkey.flush()
            cli_args = [
                "cardano-cli",
                era,
                "stake-address",
                "stake-delegation-certificate",
                "--stake-verification-key-file",
                vkey.name,
                "--stake-pool-id",
                pool_id,
                "--out-file",
                delegation_certificate.name
            ]

            if self.debug:
                print(f"def delegate_pool_tx: {" ".join(cli_args)}")

            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                raise Exception(f"Unknown error generating stake delegation certificate for account {account}")
            result = self.build_tx(account, out_file, fee, certificates=[delegation_certificate.name], ttl=ttl, sign=sign, stake=True)
        return result


    def delegate_vote_tx(self, account, voteType, voteTarget, out_file, fee, ttl=None, sign=False, era="latest"):
        with tempfile.NamedTemporaryFile("w+") as vote_delegation_certificate, tempfile.NamedTemporaryFile("w+") as vkey:
            vkey.write(self.accounts[account]["stake_vkey"])
            vkey.flush()
            cli_args = [
                "cardano-cli",
                era,
                "stake-address",
                "vote-delegation-certificate",
                f"--{voteType}",
                *([voteTarget] if voteTarget is not None else []),
                "--stake-verification-key-file",
                vkey.name,
                "--out-file",
                vote_delegation_certificate.name
            ]

            if self.debug:
                print(f"def delegate_vote_tx: {" ".join(cli_args)}")

            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                raise Exception(f"Unknown error generating vote delegation certificate for account {account}")
            result = self.build_tx(account, out_file, fee, certificates=[vote_delegation_certificate.name], ttl=ttl, sign=sign, stake=True)
        return result


    def drain_tx(self, account, send_addr, out_file, fee, multiasset=False, rewards=False, ttl=None, sign=False):
        stake_address = self.accounts[account]["stake_address"]

        if os.getenv('BLOCKFROST_DISABLE', 'False') == "True":
            rewards = os.getenv(f"ADAWALLET_ACCOUNT_{account}_STAKE_REWARDS")
            if rewards is None:
                print(f"Blockfrost is disabled, unable to fetch rewards.  Please set account {account} stake rewards via env var: ADAWALLET_ACCOUNT_{account}_STAKE_REWARDS")
                exit(1)
            else:
                print(f"Blockfrost is disabled, setting account {account} stake rewards from env var: ADAWALLET_ACCOUNT_{account}_STAKE_REWARDS")
                withdrawals = { stake_address: int(os.getenv(f"ADAWALLET_ACCOUNT_{account}_STAKE_REWARDS", "0")) }
        elif rewards:
            withdrawals = self.get_rewards_for_stake_address(stake_address)
        else:
            withdrawals = {}
            withdrawals[stake_address] = 0

        if self.debug:
            print(f"def drain_tx: rewards withdrawal are {"enabled" if rewards else "disabled"}; stake address:rewards for account {account} are: {withdrawals}")

        if withdrawals[stake_address] == 0:
            print(f"No rewards for address {stake_address} -- drain tx may still be created if account payment address UTXO are present")
            withdrawals = {}

        return self.build_tx(account, out_file, fee, withdrawals=withdrawals, ttl=ttl, sign=sign, stake=True, change_address=send_addr, multiasset=multiasset)


    def bundle_NT(self, utxo_assets):
        parts = []
        totals = defaultdict(int)

        # Sum identical native tokens to minimize required lovelace and Tx size
        for _, _, policy, asset_name, quantity in utxo_assets:
            totals[(policy, asset_name)] += int(quantity)

        # Sort by policy, then asset name
        items = list(totals.items())
        items.sort(key=lambda x: (x[0][0], x[0][1]))

        # Build and return the bundled native token string
        for (policy, asset_name), quantity in items:
            parts.append(f"{quantity} {policy}.{asset_name}" if asset_name else f"{quantity} {policy}")
        return " + ".join(parts)


    def bundle_NT_txout(self, lovelace, address, utxo_assets):
        b = self.bundle_NT(utxo_assets)
        return f"{address} + {lovelace}" if not b else f"{address} + {lovelace} + {b}"


    def get_pparams(self):
        cursor = self.db.cursor()
        row = cursor.execute("SELECT * from pparams").fetchone()
        if row is None:
            return None
        else:
            return row[0]


    def build_tx(self, account, out_file, fee, txouts={}, withdrawals={}, certificates=[], ttl=None, sign=False, deposit=0, stake=False, change_address=None, multiasset=False, asset_address=None, era="latest"):
        account_address = self.accounts[account]["address"]

        if change_address == None:
            change_address = account_address

        if asset_address == None:
            asset_address = change_address

        if not ttl:
            if os.getenv('BLOCKFROST_DISABLE', 'False') == "True":
                print(f"Blockfrost is disabled.  Please set ttl to an absolute slot height explicitly using: `--ttl <INT>`")
                exit(1)
            ttl = self.get_slot_tip() + 7200

        out_total = 0
        in_total = 0
        nt_total = 0

        cli_args = [
          "cardano-cli",
          era,
          "transaction",
          "build-raw",
          "--ttl",
          str(ttl),
          "--out-file",
          out_file
        ]

        account_utxos, account_utxo_assets = self.fetch_utxos_address(account_address, multiasset)
        pparamsText = self.get_pparams()

        if pparamsText is None and multiasset:
            print("Transactions involving native tokens require an up to date protocol-parameters file to calculate minimum native token lovelace.")
            print("From a syncronized cardano-node machine, generate such a file with cardano-cli:")
            print()
            print(f"    cardano-cli latest query protocol-parameters {" ".join(self.magic_args)} > pp.json")
            print()
            print("This file can then be imported into adawallet with:")
            print()
            print(f"    adawallet import-pparams --pparams-file pp.json")
            exit(1)

        with tempfile.NamedTemporaryFile("w+") as pparams:
            # Prepare a tmpfile with protocol parameters if available.
            if pparamsText:
                pparams.write(pparamsText)
                pparams.flush()

            if len(account_utxos) > 0:
                # Automatically add all lovelace containing UTXO for the account.
                # This will include native token associated lovelace UTXO if multiasset is true.
                for txid, index, value, _, _, _ in account_utxos:
                    cli_args.extend(["--tx-in", f"{txid}#{index}"])
                    in_total += value

                # Any custom txouts requested.
                for address, value in txouts.items():
                    # Don't create txouts for dust UTXO.
                    if value >= 1000000:
                        cli_args.extend(["--tx-out", f"{address}+{value}"])
                        out_total += value
                    else:
                        print(f"Skipping dust output UTXO {address}+{value}")

                # Any custom withdrawals requested.
                for address, value in withdrawals.items():
                    cli_args.extend(["--withdrawal", f"{address}+{value}"])
                    in_total += value

                # Any custom certificates requested.
                for certificate in certificates:
                    cli_args.extend(["--certificate", certificate])

                if multiasset:
                    nt_txout_calc = self.bundle_NT_txout(0, change_address, account_utxo_assets)

                    cli_NT_args = [
                        "cardano-cli",
                        era,
                        "transaction",
                        "calculate-min-required-utxo",
                        "--protocol-params-file",
                        pparams.name,
                        "--tx-out",
                        nt_txout_calc
                    ]

                    if self.debug:
                        print(f"def build_tx cli_NT_args: {" ".join(cli_NT_args)}")

                    p = subprocess.run(cli_NT_args, capture_output=True, text=True)
                    if p.returncode != 0:
                        print(p.stderr)
                        raise Exception(f"Unknown error calculating the minimum required lovelace UTXO for aggregate native tokens")

                    # Obtain the calculated minimum lovelace to bind the native token txout
                    match = re.search(r"(?:Coin|Lovelace)\s+(\d+)", p.stdout.strip())
                    if not match:
                        raise Exception(f"Unexpected output from cardano-cli while calculating required lovelace UTXO for aggregate native tokens: {p.stdout.strip()!r}")

                    # Assemble the final aggregated native token txout
                    nt_total = int(match.group(1))
                    nt_txout = self.bundle_NT_txout(nt_total, asset_address, account_utxo_assets)
                    cli_args.extend(["--tx-out", nt_txout])

                change = in_total - out_total - fee - deposit - nt_total
                if change >= 1000000:
                    cli_args.extend(["--tx-out", f"{change_address}+{change}"])
                elif change == 0:
                    pass
                elif change < 1000000 and change > 0:
                    fee = change + fee
                elif change < 0:
                    raise Exception("Error generating transaction, not enough funds")
                else:
                    raise Exception("Error generating transaction, unknown error calculating change")
                cli_args.extend(["--fee", str(fee)])

                if self.debug:
                    print(f"def build_tx cli_args: {" ".join(cli_args)}")

                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0:
                    print(p.stderr)
                    raise Exception("Unknown error creating build-raw transaction")

                # If protocol parameters are available, sanity check the fee.
                if pparamsText:
                    cli_fee_check_args = [
                        "cardano-cli",
                        era,
                        "transaction",
                        "calculate-min-fee",
                        "--tx-body-file",
                        out_file,
                        "--protocol-params-file",
                        pparams.name,
                        "--witness-count",
                        "2" if stake else "1"
                    ]

                    if self.debug:
                        print(f"def build_tx cli_fee_check_args: {" ".join(cli_args)}")

                    p = subprocess.run(cli_fee_check_args, capture_output=True, text=True)
                    if p.returncode != 0:
                        print(p.stderr)
                        raise Exception("Unknown error calculating minimum fee")

                    calc_fee = json.loads(p.stdout.strip())["fee"]
                    if calc_fee < fee:
                        print(f"Calculated fee is: {calc_fee}, actual fee used is: {Fore.GREEN + str(fee) + Style.RESET_ALL}")
                    else:
                        print(f"Calculated fee is: {calc_fee}, actual fee used is: {Fore.RED + str(fee) + Style.RESET_ALL} -- {Fore.YELLOW + "adjust your fee and try again!" + Style.RESET_ALL}")
                        exit(1)

                if sign:
                    self.sign_tx(account, out_file, out_file, stake=stake)

                return((in_total, out_total, fee, change))
            else:
                print(f"No UTXO for address {account_address} -- skipping tx creation")
                return (0,0,0,0)


    def get_utxos_for_address(self, address, filter_min):
        utxo_count = 0
        utxo_count_nt = 0
        utxo_count_filter_min = 0
        utxos = []
        utxo_assets = []

        if os.getenv('BLOCKFROST_DISABLE', 'False') == "True":
            print(f"Blockfrost is disabled.  Please use `adawallet import-utxos` sub-command to fetch UTXO.")
            exit(1)

        try:
            bf_utxos: List[BlockFrostUtxo] = self.blockfrost.address_utxos(address=address)
        except ApiError as e:
            if e.status_code == 404:
                print(f"blockfrost: Address {address} has no UTXO")
                return []
            else:
                print(f"Error obtaining UTXO for address {address}:")
                print(e)
                exit(1)

        for utxo in bf_utxos:
            native_token = False
            utxo_count += 1

            txid = utxo.tx_hash
            tx_index = utxo.tx_index
            data_hash = utxo.data_hash
            inline_datum = utxo.inline_datum
            ref_script_hash = utxo.reference_script_hash

            assets = utxo.amount
            assetCount = len(assets)

            for asset in assets:
                quantity = int(asset.quantity)

                if asset.unit == "lovelace":
                    # If the lovelace is in a UTXO containing native token(s),
                    # or if a lovelace only UTXO and the quantity is greater than the filter_min,
                    # record the lovelace of the UTXO.
                    if assetCount > 1 or quantity > filter_min:
                        amount = quantity
                        utxos.append(
                            (
                                txid,
                                tx_index,
                                address,
                                amount,
                                data_hash,
                                inline_datum,
                                ref_script_hash
                            )
                        )

                    # Otherwise, count a lovelace only filtered UTXO.
                    else:
                        utxo_count_filter_min += 1

                # If the UTXO asset is not lovelace.
                else:
                    policy_id = asset.unit[:56]
                    asset_name = asset.unit[56:]
                    utxo_assets.append(
                        (
                            txid,
                            tx_index,
                            policy_id,
                            asset_name,
                            quantity
                        )
                    )
                    native_token = True


            # Count a native token containing UTXO
            if native_token == True:
                utxo_count_nt += 1

        print(f"Address {address} had {len(utxos)} UTXO imported; Total parsed: {utxo_count}, Native token containing: {utxo_count_nt}, Min filtered: {utxo_count_filter_min}")
        return (utxos, utxo_assets, utxo_count_nt, utxo_count_filter_min)
