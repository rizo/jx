let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "41bf9e887fa8f1399ac328f1868d6d2ba27aab9f";
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
