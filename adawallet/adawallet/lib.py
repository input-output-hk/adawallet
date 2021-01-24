import binascii
import json
import subprocess
import tempfile
import time
import os
import sys
import apsw
import tarfile
from pathlib import Path

# openapi client for rosetta
import cardano_rosetta
from cardano_rosetta.rest import ApiException

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
    """A single address wallet library that supports menmonics and hardware wallets"""

    def __init__(self, state_dir):
        self.state_dir = Path(state_dir)
        self.accounts = {}
        self.db = None
        self.wallet_initialized = False
        self.load_state()

    def initialize(self, testnet, rosetta_url="https://explorer.cardano.org/rosetta"):
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
        cursor.execute("create table status(hw_wallet,root_key,testnet,rosetta_url)")
        cursor.execute("create table utxo(txid,tx_index,address,amount)")
        cursor.execute("create table accounts(id,payment_vkey,payment_skey,stake_vkey,stake_skey,address,stake_address)")

    def load_state(self):
        if not os.path.exists(self.state_dir / "data.sqlite"):
            return
        if os.path.exists(self.state_dir / "data.sqlite") and not self.db:
            self.db = apsw.Connection(str((self.state_dir / "data.sqlite").resolve()))
        cursor = self.db.cursor()
        row = cursor.execute("select * from status").fetchone()
        if row:
            self.wallet_initialized = True
            self.hardware_wallet = row[0]
            self.root_key = row[1]
            self.testnet = row[2]
            if row[3]:
                self.rosetta_url = row[3]
                configuration = cardano_rosetta.Configuration(
                    host = self.rosetta_url,
                )
                # uncomment to debug rosetta
                # configuration.debug = True
                self.rosetta_client = cardano_rosetta.ApiClient(configuration)
            if self.testnet:
                self.magic_args = ["--testnet-magic", "0"]
            else:
                self.magic_args = ["--mainnet"]
            rows = cursor.execute("select * from accounts")
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
            'srm',
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
        cursor.execute("insert into status values(?,?,?,?)", (True, None, self.testnet, self.rosetta_url))
        self.hardware_wallet = True

    def initialize_read_only(self):
        cursor = self.db.cursor()
        cursor.execute("insert into status values(?,?,?,?)", (False, None, self.testnet, self.rosetta_url))


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
        cursor.execute("insert into status values(?,?,?,?)", (False, root_key, self.testnet, self.rosetta_url))

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
        cursor = self.db.cursor()
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
        cursor.execute("insert into accounts values(?,?,?,?,?,?,?)", account_keys)

    def build_address(self, payment_vkey, stake_vkey):
        with tempfile.NamedTemporaryFile("w+") as payment, tempfile.NamedTemporaryFile("w+") as stake:
            payment.write(payment_vkey)
            payment.flush()
            stake.write(stake_vkey)
            stake.flush()
            cli_args = [
                "cardano-cli",
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

    def build_stake_address(self, stake_vkey):
        with tempfile.NamedTemporaryFile("w+") as stake:
            stake.write(stake_vkey)
            stake.flush()
            cli_args = [
                "cardano-cli",
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
                with tempfile.NamedTemporaryFile("w+") as payment_vkey, tempfile.NamedTemporaryFile("w+") as payment_hws, tempfile.NamedTemporaryFile("w+") as stake_vkey, tempfile.NamedTemporaryFile("w+") as stake_hws:
                    account_args.extend(["--path", f"1852H/1815H/{i}H/0/0", "--hw-signing-file", payment_hws.name, "--verification-key-file", payment_vkey.name])
                    account_args.extend(["--path", f"1852H/1815H/{i}H/2/0", "--hw-signing-file", stake_hws.name, "--verification-key-file", stake_vkey.name])
                    key_files.append((i, payment_vkey, payment_hws, stake_vkey, stake_hws))

                    cli_args = [
                        "cardano-hw-cli",
                        "shelley",
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
                            print(payment_vkey_contents)
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
                    "shelley",
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

    def sign_tx(self, account, tx_body, out_file, stake=False):
        if account not in self.accounts:
            self.import_account(account)

        if self.hardware_wallet:
            with tempfile.NamedTemporaryFile("w+") as payment_hws, tempfile.NamedTemporaryFile("w+") as stake_hws:
                signing_args = []
                payment_hws.write(self.accounts[account]["payment_skey"])
                payment_hws.flush()
                signing_args.extend(["--hw-signing-file", payment_hws.name])

                if stake:
                    stake_hws.write(self.accounts[account]["stake_skey"])
                    stake_hws.flush()
                    signing_args.extend(["--hw-signing-file", stake_hws.name])
                cli_args = [
                    "cardano-hw-cli",
                    "shelley",
                    "transaction",
                    "sign",
                    *self.magic_args,
                    "--tx-body-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    # TODO: cardano-hw-cli prints an error to stdout. Remove when fixed
                    print(p.stdout)
                    print(p.stderr)
                    raise Exception(f"Unknown error signing transaction with account {account}")
            return
        elif self.accounts[account]["payment_skey"]:
            with tempfile.NamedTemporaryFile("w+") as payment_skey, tempfile.NamedTemporaryFile("w+") as stake_skey:
                signing_args = []
                payment_skey.write(self.accounts[account]["payment_skey"])
                payment_skey.flush()
                signing_args.extend(["--signing-key-file", payment_skey.name])

                if stake:
                    stake_hws.write(self.accounts[account]["stake_skey"])
                    stake_hws.flush()
                    signing_args.extend(["--signing-key-file", stake_skey.name])
                cli_args = [
                    "cardano-cli",
                    "transaction",
                    "sign",
                    "--tx-body-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    # TODO: cardano-hw-cli prints an error to stdout. Remove when fixed
                    print(p.stdout)
                    print(p.stderr)
                    raise Exception(f"Unknown error signing transaction with account {account}")
            return
        raise Exception(f"No signing key available for account {account}")

    def bulk_witness_tx(self, tx_archive, out_file, role):
        with tarfile.open(name=out_file, mode='w:gz') as tarout, tarfile.open(name=tx_archive, mode='r:gz') as tarin:
                for member in tarin.getmembers():
                    with tempfile.NamedTemporaryFile("w+") as tx, tempfile.NamedTemporaryFile("w+") as witness:
                        with open(tx, "w") as f:
                            tarout.extractfile(f)
                        self.witness_tx(member.name.split[0], tx.name, out_file, role)

    def witness_tx(self, account, tx_body, out_file, role):
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
                    "shelley",
                    "transaction",
                    "witness",
                    *self.magic_args,
                    "--tx-body-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    # TODO: cardano-hw-cli prints an error to stdout. Remove when fixed
                    print(p.stdout)
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
                    "transaction",
                    "witness",
                    "--tx-body-file",
                    tx_body,
                    "--out-file",
                    out_file,
                    *signing_args
                ]
                p = subprocess.run(cli_args, capture_output=True, text=True)
                if p.returncode != 0 or not os.path.exists(out_file):
                    print(" ".join(cli_args))
                    # TODO: cardano-hw-cli prints an error to stdout. Remove when fixed
                    print(p.stdout)
                    print(p.stderr)
                    raise Exception(f"Unknown error witnessing transaction with account {account}")
            return
        raise Exception(f"No signing key available for account {account} with role {role}")

    def clear_utxo_table(self):
        cursor = self.db.cursor()
        cursor.execute("delete from utxo")

    def update_utxos_for_accounts(self, reload_state=False):
        self.clear_utxo_table()
        cursor = self.db.cursor()
        for account,details in self.accounts.items():
            utxos = self.get_utxos_for_address(details["address"])
            for utxo in utxos:
                cursor.execute("insert into utxo values(?,?,?,?)", utxo)
        if reload_state:
            self.load_state()

    def import_utxos_for_accounts(self, utxo_json, reload_state=False):
        self.clear_utxo_table()
        with open(utxo_json, 'r') as f:
            utxos = json.load(f)
        cursor = self.db.cursor()
        for utxo_attrs in utxos:
            utxo = (utxo_attrs['txid'], utxo_attrs['tx_index'], utxo_attrs['address'], utxo_attrs['amount'])
            cursor.execute("insert into utxo values(?,?,?,?)", utxo)
        if reload_state:
            self.load_state()

    def export_utxos_for_accounts(self, utxo_json):
        cursor = self.db.cursor()
        utxo_entries = []
        rows = cursor.execute("select * from utxo")
        for txid, tx_index, address, amount in rows:
            utxo_entries.append({
                    "txid": txid,
                    "tx_index": tx_index,
                    "address": address,
                    "amount": amount
            })
        with open(utxo_json, 'w') as f:
            f.write(json.dumps(utxo_entries))

    def get_rewards_for_stake_address(self, stake_address):
        # TODO: query for rewards when rosetta supports it
        return 0

    def get_network_list(self):
        network_api_instance = cardano_rosetta.NetworkApi(self.rosetta_client)
        metadata_request = cardano_rosetta.MetadataRequest() # MetadataRequest |
        try:
            # Get List of Available Networks
            return network_api_instance.network_list(metadata_request).network_identifiers[0]
        except ApiException as e:
            print("Exception when calling NetworkApi->network_list: %s\n" % e)

    def get_network_status(self, network_identifier):
        network_api_instance = cardano_rosetta.NetworkApi(self.rosetta_client)
        network_request = cardano_rosetta.NetworkRequest(network_identifier)
        try:
            return network_api_instance.network_status(network_request)
        except ApiException as e:
            print("Exception when calling NetworkApi->network_status: %s\n" % e)

    def get_block(self, block_identifier=None):
        network_identifier = self.get_network_list()
        if not block_identifier:
            network_status = self.get_network_status(network_identifier)
            block_identifier = network_status.current_block_identifier

        block_request = cardano_rosetta.BlockRequest(network_identifier, block_identifier)
        block_api_instance = cardano_rosetta.BlockApi(self.rosetta_client)
        try:
            return block_api_instance.block(block_request)
        except ApiException as e:
            print("Exception when calling BlockApi->block: %s\n" % e)

    def get_slot_tip(self):
        block = self.get_block()
        return int(block.block.metadata.slot_no)

    def fetch_utxos_address(self, address):
        utxos = []
        cursor = self.db.cursor()
        rows = cursor.execute("select * from utxo WHERE address=?", (address,))
        for row in rows:
            utxos.append((row[0], row[1], int(row[3])))
        return utxos

    def stake_registration_tx(self, account, out_file, fee, ttl=None, sign=False, deposit=2000000):
        with tempfile.NamedTemporaryFile("w+") as stake_registration_certificate, tempfile.NamedTemporaryFile("w+") as vkey:
            vkey.write(self.accounts[account]["stake_vkey"])
            vkey.flush()
            cli_args = [
                "cardano-cli",
                "stake-address",
                "registration-certificate",
                "--stake-verification-key-file",
                vkey.name,
                "--out-file",
                stake_registration_certificate.name
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(" ".join(cli_args))
                # TODO: cardano-hw-cli prints an error to stdout. Remove when fixed
                print(p.stdout)
                print(p.stderr)
                raise Exception(f"Unknown error generating stake registration certificate for account {account}")
            return self.build_tx(account, out_file, fee, certificates=[stake_registration_certificate], ttl=ttl, sign=sign, deposit=deposit, stake=True)

    def bulk_stake_registration_tx(self, out_file, fee, ttl=None, deposit=2000000, sign=False):
        if sign:
            suffix="txsigned"
        else:
            suffix="txbody"

        sum_result = (0, 0, 0, 0)

        with tarfile.open(name=out_file, mode='w:gz') as tar:
            for account,details in self.accounts.items():
                with tempfile.NamedTemporaryFile("w+") as tx:
                    result = self.stake_registration_tx(int(account), tx.name, fee, ttl, sign, deposit)
                    tar.add(tx, f"{account}.{suffix}")
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
                for account,pool_id in delegations.items():
                    with tempfile.NamedTemporaryFile("w+") as tx:
                        result = self.delegate_pool_tx(int(account), pool_id, tx.name, fee, ttl, sign)
                        tar.add(tx, f"{account}.{suffix}")
                    sum_result = (sum_result[0] + result[0], sum_result[1] + result[1], sum_result[2] + result[2], sum_result[3] + result[3])
        return sum_result


    def delegate_pool_tx(self, account, pool_id, out_file, fee, ttl=None, sign=False):
        with tempfile.NamedTemporaryFile("w+") as delegation_certificate, tempfile.NamedTemporaryFile("w+") as vkey:
            vkey.write(self.accounts[account]["stake_vkey"])
            vkey.flush()
            cli_args = [
                "cardano-cli",
                "stake-address",
                "delegation-certificate",
                "--stake-verification-key-file",
                vkey.name,
                "--stake-pool-id",
                pool_id,
                "--out-file",
                delegation_certificate.name
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(" ".join(cli_args))
                # TODO: cardano-hw-cli prints an error to stdout. Remove when fixed
                print(p.stdout)
                print(p.stderr)
                raise Exception(f"Unknown error generating stake delegation certificate for account {account}")
            result = self.build_tx(account, out_file, fee, certificates=[delegation_certificate], ttl=ttl, sign=sign, stake=True)
        return result

    def build_tx(self, account, out_file, fee, txouts={}, withdrawals={}, certificates=[], ttl=None, sign=False, deposit=0, stake=False):
        if not ttl:
            ttl = self.get_slot_tip() + 5000
        account_address = self.accounts[account]["address"]
        out_total = 0
        in_total = 0

        cli_args = [
          "cardano-cli",
          "transaction",
          "build-raw",
          "--ttl",
          str(ttl),
          "--out-file",
          out_file
        ]

        for txid, index, value in self.fetch_utxos_address(account_address):
            cli_args.extend(["--tx-in", f"{txid}#{index}"])
            in_total += value

        for address, value in txouts.items():
            cli_args.extend(["--tx-out", f"{address}+{value}"])
            out_total += value

        for address, value in withdrawals.items():
            cli_args.extend(["--withdrawal", f"{address}+{value}"])
            in_total += value

        for certificate in certificates:
            cli_args.extend(["--certificate", certificate])

        change = in_total - out_total - fee - deposit
        if change >= 1000000:
            cli_args.extend(["--tx-out", f"{account_address}+{change}"])
        elif change == 0:
            pass
        elif change < 1000000 and change > 0:
            fee = change + fee
        elif change < 0:
            raise Exception("Error generating transaction, not enough funds")
        else:
            raise Exception("Error generating transaction, unknown error calculating change")
        cli_args.extend(["--fee", str(fee)])

        p = subprocess.run(cli_args, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error creating bulk transaction")
        if sign:
            self.sign_tx(account, out_file, out_file, stake=stake)
        return((in_total, out_total, fee, change))

    def get_utxos_for_address(self, address):
        # Create an instance of the API class
        network_identifier = self.get_network_list()
        network_status = self.get_network_status(network_identifier)

        account_api_instance = cardano_rosetta.AccountApi(self.rosetta_client)
        account_identifier = cardano_rosetta.AccountIdentifier(address)
        block_identifier = network_status.current_block_identifier
        try:
            block = self.get_block(block_identifier)
        except ApiException as e:
            print("Exception when calling BlockApi->block: %s\n" % e)

        utxo_request = cardano_rosetta.AccountBalanceRequest(network_identifier, account_identifier, block) # BlockRequest |
        try:
            # Get a Block
            balance_request = account_api_instance.account_balance(utxo_request)
            coins = balance_request.coins
            utxos = []
            for coin in coins:
                if coin.amount.currency.symbol == 'ADA':
                    (txid, index) = coin.coin_identifier.identifier.split(":")
                    amount = coin.amount.value
                    utxos.append((txid, index, address, coin.amount.value))
            return utxos
        except ApiException as e:
            print("Exception when calling AccountApi->account_balance: %s\n" % e)
