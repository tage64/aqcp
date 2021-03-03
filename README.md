# aqcp
Advanced Quantified Communication Program

## Building

### Unix like (Linux/MacOSX)

On unix like operating systems we use the package manager [nix][2] integrated with [stack][1]. You need to install those.

* Install [stack][1]
* Install [nix][3]
* Just run:

    ```
    $ stack build
    # Or if you want to run it:
    $ stack run -- --help
    # Or to install:
    $ stack install
    ```

### Windows

* Install [stack][1]
* As described [here](https://docs.haskellstack.org/en/stable/developing_on_windows), stack comes with an installation of [msys2](https://www.msys2.org) which is a linux-like building platform for windows with a package manager called pacman. We need to use that pacman to install some packages into our stack environment before we can build this program. So run the following in your terminal:

    ```
    $ stack exec -- pacman -Suy  # Initial upgrade of msys packages.
    # You may need to close and open the terminal now.
    $ stack exec -- pacman -Suy  # All msys packages should now be uptodate.
    $ stack exec -- pacman -S mingw-w64-x86_64-toolchain  # Choose to install all packages if you're unsure.
    $ stack exec -- pacman -S mingw-w64-x86_64-cmake
    $ stack exec -- pacman -S mingw-w64-x86_64-portaudio
    $ stack exec -- pacman -S mingw-w64-x86_64-libsodium
    # And now we should just be able to run:
    $ stack --no-nix build
    # Or:
    $ stack --no-nix run -- --help
    ```

The key here on windows is that we cannot use nix so we pass --no-nix to all stack commands.

## Testing

* Test cases are provided in the folder test. To run the these use:

    ```
    $ stack test
    # Or on windows:
    $ stack --no-nix test
    ```

[1]: https://docs.haskellstack.org/en/stable/README
[2]: https://nixos.org
[3]: https://nixos.org/download.html
