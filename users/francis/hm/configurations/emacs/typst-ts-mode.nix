{
  trivialBuild,
  fetchgit,
}:
# meow_king/typst-ts-mode
trivialBuild rec {
  pname = "typst-ts-mode";
  version = "main";
  src = fetchgit {
    url = "https://codeberg.org/meow_king/typst-ts-mode.git";
    hash = "sha256-4lb3NXB++yxgaWfyxQupfD/miNxO3f4N1XHWUMpK1JE";
  };
  # elisp dependencies
  # propagatedUserEnvPkgs = [
  #   all-the-icons
  # ];
  # buildInputs = propagatedUserEnvPkgs;
}
