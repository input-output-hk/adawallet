#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3

# This script can be used to convert yarn.lock files which contain sha512
# hashes to sha1 hashes. Doing this may be required when pinning to older
# nixpkgs for a build dependent and version compatible node, yarn and yarn2nix
# package set which aren't yet compatible with sha512 hashes in yarn.lock.
#
# There are no other readily available tools for downgrading yarn.lock hashes
# when pinning to an older package set.
#
# Tips:
# * In the case that a project's yarn.lock contains a package block with
#   multiple semver requirements, not all of which can be satisfied with a single
#   package, the project can be built from nix shell with network access to see
#   which packages are resolved, and then the yarn.lock file updated to reflect
#   multiple packages as needed, each of which will be semver satisfied with a
#   single package per block.
#
# * While this script will successfully convert the majority of sha512s, there
#   may be a few that get missed and need manual investigation and patching.
#   A few helpful commands may be:
#
#     nix-prefetch-url --type sha1 $PACKAGE_URL
#     sha1sum $PACKAGE.tgz | cut -d' ' -f1 | xxd -r -p | base64

import base64
import hashlib
import re
import time
import urllib.request

lockfile_in = "yarn.lock"
lockfile_out = "yarn.lock.patched"

with open(lockfile_in, "r") as f:
    lines = f.readlines()

out_lines = []
i = 0
while i < len(lines):
    line = lines[i]
    out_lines.append(line)

    if re.match(r'^\s*integrity .*sha512-', line):
        resolved_url = None
        for j in range(i - 3, i + 3):
            if j < 0 or j >= len(lines):
                continue
            match = re.match(r'^\s{2}resolved "(https://registry\.yarnpkg\.com/.+\.tgz)(#[^"]+)?"', lines[j])
            if match:
                resolved_url = match.group(1)
                break

        if not resolved_url:
            print(f"⚠️  Could not find resolved URL for sha512-only integrity near line {i+1}")
            i += 1
            continue

        print(f"→ Fetching: {resolved_url}")
        try:
            with urllib.request.urlopen(resolved_url) as resp:
                data = resp.read()
        except Exception as e:
            print(f"❌ Failed to fetch {resolved_url}: {e}")
            i += 1
            continue

        sha1 = hashlib.sha1(data).digest()
        sha1_b64 = base64.b64encode(sha1).decode()

        new_line = f'  integrity sha1-{sha1_b64}\n'
        out_lines[-1] = new_line
        print(f"✔️  Replaced with: {new_line.strip()}")

        time.sleep(0.2)

    i += 1

with open(lockfile_out, "w") as f:
    f.writelines(out_lines)

print(f"\n✅ Wrote patched lockfile to: {lockfile_out}")

