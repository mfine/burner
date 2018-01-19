{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
  ];

  ec2.hvm = true;

  # Set your time zone.
  time.timeZone = "Universal";

  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    bash
    bc
    wget
    curl
    git
    gcc
    gnumake
    haskell.compiler.ghc7103
    htop
    jq
    cmake
    emacs
    inetutils
    vim
    stack
    silver-searcher
    unzip
    tcpdump
    screen
    socat
    which
    docker
    python27
    python27Packages.awscli
  ];

  # Run docker.
  virtualisation.docker.enable = true;

  # Setup mark user.
  users.extraUsers.mark = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    hashedPassword = "$1$lDLcVVKd$.qLbasqRcgQveT5tm2RAj1";
  };
}
