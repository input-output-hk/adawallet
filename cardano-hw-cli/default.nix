{ stdenv, lib, fetchurl, patchelf, libusb, libudev }:

let
  version = "1.2.0-rc0";
in stdenv.mkDerivation {
  pname = "cardano-hw-cli";
  version = "v${version}";
  src = fetchurl {
    url = "https://github.com/vacuumlabs/cardano-hw-cli/releases/download/v${version}/cardano-hw-cli-${version}_linux-x64.tar.gz";
    #url = "https://s3-eu-central-1.amazonaws.com/ci-static/cardano-hw-cli.tar.gz";
    sha256 = "sha256-fcpu/q0sNlO86elhphiCYJAuIWsduJtu0JrG1gdBK5g=";
  };
  phases = [ "unpackPhase" "installPhase" ];
  #sourceRoot = ".";
  installPhase = ''
    BASH_COMPLETIONS=$out/share/bash-completion/completions
    mkdir -p $out/bin
    mkdir -p "$BASH_COMPLETIONS"
    patchelf --set-interpreter ${stdenv.cc.libc}/lib/ld-linux-x86-64.so.2 cardano-hw-cli
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb libudev ]} cardano-hw-cli
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb libudev ]} Release/usb_bindings.node
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb libudev ]} Release/HID.node
    patchelf --set-rpath ${lib.makeLibraryPath [ stdenv.cc.cc libusb libudev ]} Release/HID-hidraw.node
    cp cardano-hw-cli $out/bin/cardano-hw-cli
    cp -a Release $out/bin/
    cp package.json $out/bin//package.json
    cp ${./autocomplete.sh} $BASH_COMPLETIONS/cardano-hw-cli


  '';
}
