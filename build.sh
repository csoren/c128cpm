#!/bin/sh
runcpm=`pwd`/runcpm/RunCPM/RunCPM
platform=$1
echo $platform

# Build RunCPM
cd runcpm/RunCPM
make $platform build
cd ../..

# Build ctools
cd ctools/src
make -f makefile.unx install
cd ../..

# Create build environment
rm -rf buildenv
mkdir buildenv
cd buildenv

# RunCPM
cp ../runcpm/RunCPM/RunCPM .
cp ../runcpm/CCP/CCP-*.* .

# Disk A user 0
mkdir -p A/0
cd A/0

# Copy tools from A.ZIP
unzip ../../../runcpm/DISK/A.ZIP
cp -v ../../../cpm/* .

# Upper case file names
for i in *; do mv -f "$i" "$(echo $i|tr a-z A-Z)" >/dev/null 2>&1; done

cd ../..

# Build CPM+.SYS
$runcpm <<EOF
submit cz
exit
EOF
cd A/0
dd bs=1 if=KEYCODE.BIN of=CPM+.SYS skip=0 seek=256 count=1024 conv=notrunc
rm *.ASM

cd ../../..

# Build disk image
DISTRIBUTION=releases/cpmfast

rm cpm+128.d71
ctools/bin/cformat -2 cpm+128.d71
ctools/bin/ctools cpm+128.d71 p buildenv/A/0/CPM+.SYS
for i in $DISTRIBUTION/*.*; do ctools/bin/ctools cpm+128.d71 p $i; done
ctools/bin/ctools cpm+128.d71 d
