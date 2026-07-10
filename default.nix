let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "7fbdec45e3c6f8db2eb5fa0c41fcd94c67babf37";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-base-compiler" = "<5.4";
    "ocamlformat" = "*";
    "ocaml-lsp-server" = "*";
    "odoc" = "*";
  };

  overlay = self: super: {
    js_of_ocaml-compiler = super.js_of_ocaml-compiler.overrideAttrs (oldAttrs: {
    # NOTE: Workaround for two issues:
    # 1. "cmdliner", used in "build" is not declared as a build dep;
    # 2. "cmdliner" attempts to update the ".install" file which is read-only.
      buildPhase = ''
        dune build -p js_of_ocaml-compiler @install
      '';
    });
  };
}
