# Nix

Nix is required to build the different components for this project and some of its runtime dependencies. This document will guide you through the necessary installation and setup steps.

## Installation

If you do not have Nix installed, please refer to [the documentation](https://nixos.org/download.html).

**Note**: Nix v2.4 or greater **is required**.

## Configuration

Edit your Nix configuration (located in `etc/nix/nix.conf` or `~/.config/nix/nix.conf`) to contain all of the following:

```
experimental-features = nix-command flakes ca-references
substituters = https://hydra.iohk.io https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```
