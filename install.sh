#!/usr/bin/env bash

set -e

if [ -f /etc/os-release ]
  then
    . /etc/os-release
    case $NAME in
      "Ubuntu") apt-get install  git omake libfuse-dev camlidl libpcre-ocaml-dev libbatteries-ocaml-dev ;;
      "Debian") aptitude install git omake libfuse-dev camlidl libpcre-ocaml-dev libbatteries-ocaml-dev ;;
      "Arch") pacman -S git omake-git ocamlfuse-cvs ocaml-pcre ocaml-batteries ;;
      "Fedora") yum install git fuse-devel ocaml-pcre-devel ocaml-findlib-devel ocaml-camomile ocaml-camlidl ocaml-bisect ocaml-ounit ocaml-ocamldoc ;;
    esac
  else
    echo "Cannot recognize your distro. Open an issue"
    exit 1
fi

install_dir=/opt/git-fs &&
rm -rf $install_dir &&
git clone https://github.com/g2p/git-fs.git $install_dir --depth 1 &&
cd $install_dir &&
git submodule update --init &&
make -C deps/ocamlfuse/lib || make -C deps/ocamlfuse/lib byte-code-library &&
omake &&
ln -sf ~+/git-fs -t /usr/bin &&
echo "git-fs installed successfully :)"
