let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "969b4582eb7248330cf07d602723a26d7b4f4b71";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-base-compiler" = "5.2.1";
    "ocamlformat" = "*";
    "ocaml-lsp-server" = "*";
    "odoc" = "*";
  };
}
