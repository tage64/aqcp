# aqcp
Advanced Quantified Communication Program

## Building

### Linux

* Install [stack](https://docs.haskellstack.org/en/stable/README/)
* Install the portaudio package with your favorite package manager to get the necessary header files.
* Just run:

    ```
    $ stack build
    ```

### Windows

* Install [stack](https://docs.haskellstack.org/en/stable/README/)
* As described [here](https://docs.haskellstack.org/en/stable/developing_on_windows), stack comes with an installation of [msys2](https://www.msys2.org) which is a linux-like building platform for windows with a package manager called pacman. We need to use that pacman to install some packages into our stack environment before we can build this program. So run the following in your terminal:

    ```
    $ stack exec -- pacman -Suy  # Initial upgrade of msys packages.
    # You may need to close and open the terminal now.
    $ stack exec -- pacman -Suy  # All msys packages should now be uptodate.
    $ stack exec -- pacman -S mingw-w64-x86_64-toolchain  # Choose to install all packages if you're unsure.
    $ stack exec -- pacman -S mingw-w64-x86_64-cmake
    $ stack exec -- pacman -S mingw-w64-x86_64-portaudio
    # And now we should just be able to run:
    $ stack build
    ```
