# aqcp
Advanced Quantified Communication Program

Aqcp is an encrypted voice chat. Currently only between two users. No central server is required, but one of the users must act as server.

## Building

### Unix like (Linux/MacOSX)

On unix like operating systems we use the package manager [nix][2] integrated with [stack][1]. You need to install those.

* Install [stack][1]
* [Install nix][3]
    * On linux:

        ```
        $ curl -L https://nixos.org/nix/install | sh
        ```

    * On some versions of Mac OS X you instead need to run:

        ```
        $ sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
        ```

    * You might need to logout and login to the shell now as suggested by the install script in order for the nix environment to be properly loaded.
* And now you should just be able to run:

    ```
    $ stack build
    # Or if you want to run it:
    $ stack run -- --help
    # Or to install:
    $ stack install
    ```

#### **Important: If stack build or stack run failes**

You can try:
```
$ rm -r ~/.stack/setup-exe-cache
```
And rerun the command.

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

* Test cases are provided in the folder test. To run these use:

    ```
    $ stack test
    # Or on windows:
    $ stack --no-nix test
    ```

## Usage

The program can either be installed via `stack install` or run directly with stack using `stack run --`.

### Get command line interface help

To get help about the command line interface run:
```
$ aqcp --help  ## If installed
$ stack run -- --help  # On unix
$ stack --no-nix -- --help  # On windows
```

### Running a server

You must forward a port and fetch your local ip address (instructions [here][4]). Then you and the client must agree upon a code which will function as meeting id.

```
$ aqcp server <IP_ADDRESS> <PORT> <CODE>
# Or
$ stack run -- server <IP_ADDRESS> <PORT> <CODE>
# Or on windows
$ stack --no-nix run -- server <IP_ADDRESS> <PORT> <CODE>
```

### Running a client

You must no the server's public ip address. The server can fetch it from <https://ipecho.net>. You must also know the server's port and the code (meeting id).

```
$ aqcp client <IP_ADDRESS> <PORT> <CODE>
# Or
$ stack run -- client <IP_ADDRESS> <PORT> <CODE>
# Or on windows
$ stack --no-nix run -- client <IP_ADDRESS> <PORT> <CODE>
```


[1]: https://docs.haskellstack.org/en/stable/README
[2]: https://nixos.org
[3]: https://nixos.org/download.html
[4]: https://ipecho.net/localip.html
