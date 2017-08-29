# CP/M for the Commodore 128

This repository contains scripts, tools and source needed to build a Commodore 128 CP/M system disk.

You're viewing the *master* branch, which contains Kerberos SRAM expansion support, plus fixes and speedups for 80 column mode from the *cpmfast* release by Linards Ticmanis.

Currently only CPM+.SYS is built from source, while the rest of the binaries are taken from the updated cpmfast release.

[Download CP/M](releases/cpm+128.d71) for the Commodore 128

[More information](releases/CPMFAST.md) on the cpmfast modifications.

## Building

### macOS

    $ ./build-osx.sh

If everything goes well, a new image cpm+128.d71 is created.

### Other unixes - Linux, WSL (Windows 10)

It's possible to use the Windows Subsystem for Linux to build. Tested with Ubuntu on Windows 10, you'll need the following packages

    $ apt-get install build-essential libreadline-dev libncurses-dev unzip

Then just run

    $ ./build-unix.sh
    
