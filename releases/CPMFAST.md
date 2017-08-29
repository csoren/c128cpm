1999-08-28

This is the speed-improved and generally-fixed CP/M distribution for the
C128. If you just want to use it, use StarCommander to transfer the
CPMFAST.D71 to a 1571 disk and you're all set! If you don't have a 1571
that you can use with StarCommander (e.g. you only have a 128D and no
external 1571) type the following in this directory:

    src\cformat -1 cpmfast.d64
    src\ctools cpmfast.d64 p src\cpm#.sys
    src\ctools cpmfast.d64 r cpm#.sys cpm+.sys
    src\ctools cpmfast.d64 p src\ccp.com

And then transfer cpmfast.d64 to your 1541. You'll need to copy the files
to a 1571 disk from inside CP/M then and add the utilities yourself.
Unfortunately, this way you don't get the Y2K fixes.

Now some detail. Features include:

1. Most importantly, vastly improved screen output speed. This is on 80col
    output only. 40col still works but is even slower than before. (so what?)
    To reach this goal, numerous shortcuts have been cut through the maze of
    twisty little paths that the original device handling code is. It is
    about (I'd guess) 75% faster than before, which makes the difference
    between curiousity and usability for e.g. WordStar.
2. More disk buffers. The CCP backup location is now in bank 1, at $e000
    under the common area. All free memory in bank 1 (about 26k) is used as
    disk buffers.
3. Better printer handling. Programs can now check if a printer is attached
    in accordance with the standard CP/M busy check method. Previously,
    it used to always return "printer active". Form feeds and vertical tabs
    now correctly flush the buffer (i.e. "pause between pages" printing
    in WordStar works now.) If a program sends data to a non-existing
    printer, the data now just vanishes instead of the machine locking
    up.
    NOTE: The system still can't notice the case when a printer is
    attached, switched on, but set to offline. It is a "feature" of
    the Commodore kernel (6502 code for IEC bus handling) that the
    machine will lock up until the printer is either switched off or
    set back to online. This occurs in native mode too.
    NOTE: It should be easy to add a User-Port printer driver if you need
    one. Since I don't have that setup I couldn't do it myself.
    I'd recommend adding another device, so users can choose with DEVICE.COM
    which one to use. Please contact me if you plan to do this.
4. Switchable German keyboard and character set. Press ASCII/DIN
    (CAPS LOCK on an English-language keyboard) to flip between the two,
    just as you would do in native mode. This works at any time a program
    is scanning the keyboard (i.e. waiting for input), without disrupting
    operation in any way. The source code is easily modifiable for another
    language, just check CXKEY.ASM, you have to modify the "umlauts" table
    and the code that uses it, it's not difficult to understand if you know
    8080 assembly at all. Feel free to ask me for help if you want to do
    another language version.
    NOTE: The German character set corresponds to ISO 646, i.e. the umlauts
    replace things like brackets and braces among others. It does NOT
    corresond to the MS-DOS 8 bit character set which is not the standard
    for CP/M systems. This standard should be followed in new versions too!!
    NOTE 2: The keyboard assignment is a bit different from before. It was
    made to match as much as possible the one from the native mode, and be
    otherwise easy to remember. Also ALL ctrl-characters can now be produced
    fromt the keyboard, even odd ones like CTRL-^ and CTRL-\. The function
    keys are preset for WordStar 4, they have the same effects as they have
    in the MS-DOS version of WordStar 4.
    NOTE 3: You can't KEYFIG the german keyboard directly. Don't try!
    You'll mess up your American keyboard! It is possible this way:
    Make a copy of cpm+.sys, keyfig it with the german keyboard, extract
    to PC (StarCommander, ctools), rename to cpmgerm.sys. Do same for
    other cpm+.sys (with American keyboard), rename to cpmengl.sys. Use the
    BASIC tools extrkycd.bas and inskycd.bas with QuickBasic or QBasic, or
    rewrite in another Basic (they're very short and easy to understand -
    you could even make a MBASIC version so you don't need a PC at all), to
    extract the keycode.bin from both, combine it into one file, and then
    reinsert into the cpmengl.sys. Move back to C128 and rename to cpm+.sys.
    Now you're done. (phew!)
5. Obviates need for dusting... well no. But it does obviate the need for
    CONF, SCREEN40 and C1571. Everything is set to sane default values,
    40col is turned off if you boot in 80col, the boot sektor of the
    distribution disk will automatically switch off verify on drive A,
    (provided you have either a C128DCR or a newer 1571, with ROM 5. This
    is recommended anyway as MFM disk handling in CP/M becomes much faster.)
6. Well-tested! This code has been used for many months now on my C128 and
    I haven't yet noticed problems or incompatibilities. The interrupt code
    has been left unmodified, just to be sure.
    NOTE: Documentation is less good, though. If you want to understand the
    changes, do a DIFF on the sources against the originals. If you want to
    understand the build process, you should know it was done on the MYZ80
    emulator. run MAKE.BAT, then once in the emulator import CZ.SUB to A:
    and SUBMIT it. The rest will be automatic, provided you have all the
    necessary tools, which are quite a few - Mac, Rmac, Gencpm, Addbios, Link
    on the CP/M side of things, MYZ80 (set up for CP/M 3!!), QBasic,
    (or QuickBasic), cformat, ctools, and StarCommander on the PC side.
    With the exception of the three large ones, i.e. MYZ80, QBasic and
    StarCommander, the rest is supplied in this package. I've also included
    the other DR development tools, i.e. hexcom, sid, lib, and xref, just
    for completeness's sake.
7. All the programs have been updated with freshly-assembled ones from the
    Caldera CP/M source distribution. All Y2K issues should be fixed! The
    date can now be displayed/entered in US, UK or YMD format. Use
    "SETDEF [UK]" or "SETDEF [YMD]" to use this feature. Thanks to whoever
    did the fixes! (the binary ZIP didn't have a name in it.)
    The datec.rsx has been applied to date.com, so DATE C works correctly
    with the new version of DATE.COM too.
    NOTE: All programs have been set to SYS to make using from other user
    numbers possible. If DIR doesn't show anything, try DIRSYS.

Any questions, comments, etc, should go to:
ticmanis@coli.uni-sb.de

NOTE TO ARCHIVE MAINTAINERS: All the other CP/M system disks are in
pub/cbm/demodisks/c128/, so this file should either go there or have a link
there and go in pub/cpm/sys/c128/system/ - this is probably the better
solution.


Linards Ticmanis




