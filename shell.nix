(import ./default.nix).shellFor {
  tools = {
    cabal = "latest";
    stack = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
