import binascii
import json
import subprocess
import tempfile
import time
import os
import sys
import apsw
from pathlib import Path

# openapi client for rosetta
import openapi_client
from openapi_client.rest import ApiException

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
            self.rosetta_url = row[3]
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

    def read_key_file(self, filename):
        with open(filename, "r") as fname:
            return fname.read()

    def write_key_file(self, name, contents):
        with open(name, "w") as f:
            f.write(contents)

    def import_accounts(self, start, end):
        if self.hardware_wallet:
            # HW wallet bulk import with one click
            self.import_accounts_hw(start, end, reload_state=False)
        else:
            for i in range(start, end + 1):
                self.import_account(i, reload_state=False)
        self.load_state()

    def import_accounts_hw(self, start, end, reload_state=True):
        account_indexes = range(start, end + 1)
        accounts = self.derive_account_keys_bulk_hw(account_indexes)
        for account_keys in accounts:
            self.write_account(account_keys)
        if reload_state:
            self.load_state()

    def import_account(self, account, reload_state=True):
        if account not in self.accounts:
            payment_vkey, payment_skey = self.derive_account_keys(account, "payment")
            stake_vkey, stake_skey = self.derive_account_keys(account, "stake")
            address = self.build_address(payment_vkey, stake_vkey)
            stake_address = self.build_stake_address(stake_vkey)
            account_keys = (account, payment_vkey, payment_skey, stake_vkey, stake_skey, address, stake_address)
            self.write_account(account_keys)
        if reload_state:
            self.load_state()

    def write_account(self, account_keys):
        cursor = self.db.cursor()
        cursor.execute("insert into accounts values(?,?,?,?,?,?,?)", account_keys)

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

    def derive_account_keys_bulk_hw(self, account_indexes):
        if self.hardware_wallet:
            accounts = []
            account_args = []
            key_files = []
            for i in account_indexes:
                (payment_vkey_handle, payment_vkey) = tempfile.mkstemp()
                (payment_hws_handle, payment_hws) = tempfile.mkstemp()
                (stake_vkey_handle, stake_vkey) = tempfile.mkstemp()
                (stake_hws_handle, stake_hws) = tempfile.mkstemp()
                account_args.extend(["--path", f"1852H/1815H/{i}H/0/0", "--hw-signing-file", payment_hws, "--verification-key-file", payment_vkey])
                account_args.extend(["--path", f"1852H/1815H/{i}H/2/0", "--hw-signing-file", stake_hws, "--verification-key-file", stake_vkey])
                key_files.append((i, payment_vkey_handle, payment_vkey, payment_hws_handle, payment_hws, stake_vkey_handle, stake_vkey, stake_hws_handle, stake_hws))

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
                os.close(payment_vkey_handle)
                os.close(payment_hws_handle)
                os.unlink(payment_vkey)
                os.unlink(payment_hws)
                os.close(stake_vkey_handle)
                os.close(stake_hws_handle)
                os.unlink(stake_vkey)
                os.unlink(stake_hws)
                raise Exception(f"Unknown error extracting bulk accounts from hardware wallet")
            for account,payment_vkey_handle, payment_vkey, payment_hws_handle, payment_hws, stake_vkey_handle, stake_vkey, stake_hws_handle, stake_hws in key_files:
                if account not in self.accounts:
                    payment_hws_contents = self.read_key_file(payment_hws)
                    payment_vkey_contents = self.read_key_file(payment_vkey)
                    stake_hws_contents = self.read_key_file(stake_hws)
                    stake_vkey_contents = self.read_key_file(stake_vkey)
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

    def witness_tx(self, account, tx_body, out_file, role):
        if account not in self.accounts:
            self.import_account(account)

        if self.hardware_wallet:
            (hws_handle, hws) = tempfile.mkstemp()
            signing_args = []
            self.write_key_file(hws, self.accounts[account][f"{role}_skey"])
            signing_args.extend(["--hw-signing-file", hws])

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
                os.close(hws_handle)
                os.unlink(hws)
                raise Exception(f"Unknown error witnessing transaction with account {account}")
            os.close(hws_handle)
            os.unlink(hws)
            return
        else:
            (skey_handle, skey) = tempfile.mkstemp()
            signing_args = []
            self.write_key_file(skey, self.accounts[account][f"{role}_skey"])
            signing_args.extend(["--signing-key-file", skey])

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
                os.close(skey_handle)
                os.unlink(skey)
                raise Exception(f"Unknown error witnessing transaction with account {account}")
            os.close(skey_handle)
            os.unlink(skey)
            return

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

    def get_utxos_for_address(self, address):
        # Defining the host is optional and defaults to http://localhost
        # See configuration.py for a list of all supported configuration parameters.
        configuration = openapi_client.Configuration(
            host = self.rosetta_url,
        )
        configuration.debug = True
        # Enter a context with an instance of the API client
        with openapi_client.ApiClient(configuration) as api_client:
            # Create an instance of the API class
            network_api_instance = openapi_client.NetworkApi(api_client)
            account_api_instance = openapi_client.AccountApi(api_client)
            metadata_request = openapi_client.MetadataRequest() # MetadataRequest |

            try:
                # Get List of Available Networks
                network_identifier = network_api_instance.network_list(metadata_request).network_identifiers[0]
            except ApiException as e:
                print("Exception when calling NetworkApi->network_list: %s\n" % e)
            network_request = openapi_client.NetworkRequest(network_identifier)
            try:
                network_status = network_api_instance.network_status(network_request)
            except ApiException as e:
                print("Exception when calling NetworkApi->network_status: %s\n" % e)

            account_identifier = openapi_client.AccountIdentifier(address)
            block_identifier = network_status.current_block_identifier
            utxo_request = openapi_client.AccountBalanceRequest(network_identifier, account_identifier, block_identifier) # BlockRequest |
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
