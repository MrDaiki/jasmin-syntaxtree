with import <nixpkgs>{};


mkShell{
    packages = with ocamlPackages; [ocaml findlib dune_3 jasmin-compiler cmdliner angstrom];
}
