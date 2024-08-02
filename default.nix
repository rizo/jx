let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "c2ed6bd430931f344a66663c9fdad25678917872";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-system" = "*";
    "ocamlformat" = "*";
    "ocaml-lsp-server" = "*";
    "odoc" = "*";
  };
}
