#!/bin/sh
runcpm=`pwd`/runcpm/RunCPM/RunCPM

# Build RunCPM
cd runcpm/RunCPM
make macosx build
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
for i in *; do mv -f "$i" "$(echo $i|tr a-z A-Z)"; done

cd ../..

$runcpm <<EOF
submit cz
exit
EOF

cd ..

DISTRIBUTION=releases/system_1987.622-3297432
FILES="CCP.COM HELP.COM HELP.HLP CONF.COM CONF.HLP KEYFIG.COM KEYFIG.HLP C1571.COM COPYSYS.COM FORMAT.COM SCREEN40.COM PIP.COM READ.ME"  

rm cpm+128.d64
ctools/bin/cformat -1 cpm+128.d64
ctools/bin/ctools cpm+128.d64 p buildenv/A/0/CPM+.SYS
for i in $FILES; do ctools/bin/ctools cpm+128.d64 p $DISTRIBUTION/$i; done
ctools/bin/ctools cpm+128.d64 d
