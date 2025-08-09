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

      mkdocs-windmill
      # mkdocs-bootswatch
      # mkdocs-custommill
      # mkdocs-material
      # mkdocs-bootstrap386
      # mkdocs-shadcn

      mkdocs-awesome-nav
      mkdocs-include-markdown-plugin
      mkdocs-git-revision-date-localized-plugin
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

    docs-redev.exec = ''
      devenv update && docs-dev
    '';

    hello.exec = ''
      echo hello from $GREET
    '';
  };

  enterShell = ''
    hello
    git --version
    pip install -e ~/Code/cloned/mkdocs-terminal
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
