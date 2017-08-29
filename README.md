# CP/M for the Commodore 128

This repository contains scripts, tools and source needed to build a Commodore 128 CP/M system disk.

You're viewing the *cpmfast* branch, which contains fixes and speedups for 80 column mode.

Currently only CPM+.SYS is built from source, while the rest of the binaries are taken from the updated cpmfast release.

## Building

### macOS

    $ ./build-osx.sh

If everything goes well, a new image cpm+128.d71 is created.

### Other unixes - Linux, WSL (Windows 10)

It's possible to use the Windows Subsystem for Linux to build. Tested with Ubuntu on Windows 10, you'll need the following packages

    $ apt-get install build-essential libreadline-dev libncurses-dev unzip

Then just run

    $ ./build-unix-sh
<<<<<<< HEAD
    
=======
    
>>>>>>> develop
