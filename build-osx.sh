#!/bin/sh
runcpm=`pwd`/runcpm/RunCPM/RunCPM

# Build RunCPM
cd runcpm/RunCPM
make macosx build
cd ../..

# Create build environment
rm -rf buildenv
mkdir buildenv
cd buildenv

# RunCPM
cp ../runcpm/RunCPM/RunCPM .
cp ../runcpm/CCP/CCP-CCPZ.* .

# Disk A user 0
mkdir -p A/0
cd A/0

# Copy tools from A.ZIP
unzip ../../../runcpm/DISK/A.ZIP
cp -v ../../../cpm/* .

# Build sub
cp ../../../build.sub .

# Upper case file names
for i in *; do mv -f "$i" "$(echo $i|tr a-z A-Z)"; done

cd ../..

$runcpm <<EOF
submit cz
exit
EOF

cd ..