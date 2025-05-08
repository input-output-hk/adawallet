{ stdenv, lib, fetchurl, patchelf, libusb1, udev }:

let
  version = "1.9.1";
in stdenv.mkDerivation {
  pname = "cardano-hw-cli";
  version = "v${version}";
  src = fetchurl {
    url = "https://github.com/vacuumlabs/cardano-hw-cli/releases/download/v${version}/cardano-hw-cli-${version}_linux-x64.tar.gz";

    sha256 = "sha256-e08CKVdvMWdmM/qhtH9xRusWfkxp52B2fYt30+Su9G0=";
  };
  phases = [ "unpackPhase" "installPhase" ];
  #sourceRoot = ".";
  installPhase = ''
    BASH_COMPLETIONS=$out/share/bash-completion/completions
    mkdir -p $out/bin
    mkdir -p "$BASH_COMPLETIONS"
    patchelf --set-interpreter ${stdenv.cc.libc}/lib/ld-linux-x86-64.so.2 cardano-hw-cli
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb1 udev ]} cardano-hw-cli
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb1 udev ]} Release/usb_bindings.node
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb1 udev ]} Release/HID.node
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb1 udev ]} Release/HID-hidraw.node
    cp cardano-hw-cli $out/bin/cardano-hw-cli
    cp -a Release $out/bin/
    cp package.json $out/bin//package.json
    cp ${./autocomplete.sh} $BASH_COMPLETIONS/cardano-hw-cli


  '';
}
