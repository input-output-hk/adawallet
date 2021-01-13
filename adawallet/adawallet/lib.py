import binascii
import json
import subprocess
import tempfile
import time
import os
import sys
import apsw
from pathlib import Path

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

    def initialize(self, testnet):
        self.testnet = testnet
        self.state_dir.mkdir(parents=True, exist_ok=True)
        db_exists = os.path.exists(self.state_dir / "data.sqlite")
        self.db = apsw.Connection(str((self.state_dir / "data.sqlite").resolve()))
        if not db_exists:
            self.initialize_db()
        self.load_state()

    def initialize_db(self):
        cursor = self.db.cursor()
        cursor.execute("create table status(hw_wallet,root_key,testnet)")
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
        cursor.execute("insert into status values(?,?,?)", (True, None, self.testnet))
        self.hardware_wallet = True

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
        cursor.execute("insert into status values(?,?,?)", (False, root_key, self.testnet))

    def read_key_file(self, filename):
        with open(filename, "r") as fname:
            return fname.read()

    def write_key_file(self, name, contents):
        with open(name, "w") as f:
            f.write(contents)

    def import_accounts(self, start, end):
        for i in range(start, end + 1):
            self.import_account(i, reload_state=False)
        self.load_state()

    def import_account(self, account, reload_state=True):
        if account not in self.accounts:
            payment_vkey, payment_skey = self.derive_account_keys(account, "payment")
            stake_vkey, stake_skey = self.derive_account_keys(account, "stake")
            address = self.build_address(payment_vkey, stake_vkey)
            stake_address = self.build_stake_address(stake_vkey)
            cursor = self.db.cursor()
            cursor.execute("insert into accounts values(?,?,?,?,?,?,?)", (account, payment_vkey, payment_skey, stake_vkey, stake_skey, address, stake_address))
        if reload_state:
            self.load_state()

    def build_address(self, payment_vkey, stake_vkey):
        (payment_handle, payment) = tempfile.mkstemp()
        (stake_handle, stake) = tempfile.mkstemp()
        self.write_key_file(payment, payment_vkey)
        self.write_key_file(stake, stake_vkey)
        cli_args = [
            "cardano-cli",
            "address",
            "build",
            *self.magic_args,
            "--payment-verification-key-file",
            payment,
            "--stake-verification-key-file",
            stake
        ]
        p = subprocess.run(cli_args, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            os.close(payment_handle)
            os.close(stake_handle)
            os.unlink(payment)
            os.unlink(stake)
            raise Exception(f"Unknown error building address")
        os.close(payment_handle)
        os.close(stake_handle)
        os.unlink(payment)
        os.unlink(stake)
        return p.stdout.rstrip()

    def build_stake_address(self, stake_vkey):
        (stake_handle, stake) = tempfile.mkstemp()
        self.write_key_file(stake, stake_vkey)
        cli_args = [
            "cardano-cli",
            "stake-address",
            "build",
            *self.magic_args,
            "--stake-verification-key-file",
            stake
        ]
        p = subprocess.run(cli_args, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception(f"Unknown error building stake address")
        os.close(stake_handle)
        os.unlink(stake)
        return p.stdout.rstrip()

    def derive_account_keys(self, account, role):
        if role == "stake":
            role_index = 2
        elif role == "payment":
            role_index = 0
        else:
            raise Exception(f"Role {role} not supported!")
        if self.hardware_wallet:
            (vkey_handle, vkey) = tempfile.mkstemp()
            (hws_handle, hws) = tempfile.mkstemp()
            cli_args = [
                "cardano-hw-cli",
                "shelley",
                "address",
                "key-gen",
                "--path",
                f"1852H/1815H/{account}H/{role_index}/0",
                "--hw-signing-file",
                hws,
                "--verification-key-file",
                vkey
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                os.close(vkey_handle)
                os.close(hws_handle)
                os.unlink(vkey)
                os.unlink(hws)
                raise Exception(f"Unknown error extracting account key {account}")
            hws_contents = self.read_key_file(hws)
            vkey_contents = self.read_key_file(vkey)
            os.close(vkey_handle)
            os.close(hws_handle)
            os.unlink(vkey)
            os.unlink(hws)
            return (vkey_contents, hws_contents)
        else:
            (vkeyx_handle, vkeyx) = tempfile.mkstemp()
            (vkey_handle, vkey) = tempfile.mkstemp()
            (skey_handle, skey) = tempfile.mkstemp()
            cli_args = [
                "cardano-address",
                "key",
                "child",
                f"1852H/1815H/{account}H/{role_index}/0",
            ]
            p = subprocess.run(cli_args, input=self.root_key, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                os.close(vkey_handle)
                os.close(skey_handle)
                os.unlink(vkey)
                os.unlink(skey)
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
                skey
            ]
            p = subprocess.run(cli_args, input=child_skey, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                os.close(skey_handle)
                os.unlink(skey)
                raise Exception(f"Unknown error converting child key to CLI format for {account}")

            cli_args = [
                "cardano-cli",
                "key",
                "verification-key",
                "--signing-key-file",
                skey,
                "--verification-key-file",
                vkeyx
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
                vkeyx,
                "--verification-key-file",
                vkey
            ]
            p = subprocess.run(cli_args, capture_output=True, text=True)
            if p.returncode != 0:
                print(p.stderr)
                raise Exception(f"Unknown error converting verification extended key to verification key for {account}")
            skey_contents = self.read_key_file(skey)
            vkey_contents = self.read_key_file(vkey)
            os.close(vkey_handle)
            os.close(vkeyx_handle)
            os.close(skey_handle)
            os.unlink(vkey)
            os.unlink(vkeyx)
            os.unlink(skey)
            return (vkey_contents, skey_contents)

    def sign_tx(self, account, tx_body, out_file, stake):
        if account not in self.accounts:
            self.import_account(account)

        if self.hardware_wallet:
            (payment_hws_handle, payment_hws) = tempfile.mkstemp()
            (stake_hws_handle, stake_hws) = tempfile.mkstemp()
            signing_args = []
            self.write_key_file(payment_hws, self.accounts[account]["payment_skey"])
            signing_args.extend(["--hw-signing-file", payment_hws])

            if stake:
                self.write_key_file(stake_hws, self.accounts[account]["stake_skey"])
                signing_args.extend(["--hw-signing-file", stake_hws])
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
                os.close(payment_hws_handle)
                os.close(stake_hws_handle)
                os.unlink(payment_hws)
                os.unlink(stake_hws)
                raise Exception(f"Unknown error signing transaction with account {account}")
            os.close(payment_hws_handle)
            os.close(stake_hws_handle)
            os.unlink(payment_hws)
            os.unlink(stake_hws)
            return
        else:
            (payment_skey_handle, payment_skey) = tempfile.mkstemp()
            (stake_skey_handle, stake_skey) = tempfile.mkstemp()
            signing_args = []
            self.write_key_file(payment_skey, self.accounts[account]["payment_skey"])
            signing_args.extend(["--signing-key-file", payment_skey])

            if stake:
                self.write_key_file(stake_hws, self.accounts[account]["stake_skey"])
                signing_args.extend(["--signing-key-file", stake_skey])
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
                os.close(payment_skey_handle)
                os.close(stake_skey_handle)
                os.unlink(payment_skey)
                os.unlink(stake_skey)
                raise Exception(f"Unknown error signing transaction with account {account}")
            os.close(payment_skey_handle)
            os.close(stake_skey_handle)
            os.unlink(payment_skey)
            os.unlink(stake_skey)
            return
