# CP/M for the Commodore 128

This repository contains scripts, tools and source needed to build a Commodore 128 CP/M system disk.

You're viewing the *cpmfast* branch, which contains fixes and speedups for 80 column mode.

Currently only CPM+.SYS is built from source, while the rest of the binaries are taken from the updated cpmfast release.

## Building

### macOS

    $ ./build-osx.sh

If everything goes well, a new image cpm+128.d71 is created.