# ocaml/opam post create script

sudo chown -R opam: _build

opam init -a --shell=zsh

# update default ocaml remote - make sure that opam finds latest package versions
# (e.g. otherwise alcotest latest version is 1.1.0 instead of 1.2.1)
opam remote remove --all default
opam repository add default --all-switches --set-default https://opam.ocaml.org

opam install --with-test --with-doc --deps-only -y .

make deps
