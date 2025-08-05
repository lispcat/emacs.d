{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [ pkgs.git ];

  # https://devenv.sh/languages/
  languages.python = {
    enable = true;
    venv.enable = true;
    venv.requirements = ''
      mkdocs
      mkdocs-material
      mkdocs-include-markdown-plugin
      mkdocs-bootstrap386
      mkdocs-git-revision-date-localized-plugin
      mkdocs-awesome-nav
      git+https://github.com/lispcat/mkdocs-terminal.git
    '';
  };
      # mkdocs-terminal

  # https://devenv.sh/processes/
  # processes.cargo-watch.exec = "cargo-watch";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  scripts = {
    docs-dev.exec = ''
      mkdocs serve
    '';

    docs-build.exec = ''
      mkdocs build
    '';

    docs-deploy.exec = ''
      mkdocs gh-deploy --force
    '';

    hello.exec = ''
      echo hello from $GREET
    '';
  };

  enterShell = ''
    hello
    git --version
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    docs-dev
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
