{ pkgs, ... }:

{
  security.acme = {
    email = "francis+mail@begyn.be"; 
    acceptTerms = true;
  };
}
