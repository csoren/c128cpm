

	title	'8502 drivers        4 Mar 86'

	maclib	x6502

	maclib	z80

	maclib	cxequ

$-MACRO
;
;      COMMON EQUATES
;
; page 0 variables, from 0a to 8f are usable
;
prtno		equ	0000Ah		; 0Ah
second$adr	equ	prtno+1		; 0Bh
DATCHN		equ	second$adr+1	; 0Ch
CMDCHN		equ	datchn+1	; 0Dh
DEVNO		equ	cmdchn+1	; 0Eh
adr$1		equ	devno+1		; 0Fh
temp$byte	equ	adr$1+2		; 11h
;		equ	temp$byte+1	; 12h

pal$nts		equ	00a03h		; FF=PAL=50Hz 0=NTSC=60Hz
serial		equ	00a1ch
d2pra		equ	0dd00h		; serial control port (clk and data)
d1sdr		equ	0dc0ch		; Fast serial data reg.
d1icr		equ	0dc0dh		; serial channel interrupt control reg

clkbit		equ	10h		; d2pra clock bit mask
;
;	KERNAL EQUATES
;
K$spin$spout	equ	0FF64h		; C=0 spin  C=1 spout

K$setbnk	equ	0FF68h		; set the logical bank # for open
					;  disk commands
					;I A=load and store bank # (C128 type bank)
					;  X=file name bank #

K$readst	equ	0FFB7h		; read status byte
					;O A = status

K$setlfs	equ	0FFBAh		; setup a logical file
					;I A=logical file #
					;  X=device # (0-31)
					;  Y=seconday command (FF if nane)

K$setnam	equ	0FFBDh		; set up file name for OPEN
					;I A=name length
					;  X=low byte pointer to name
					;  Y=high byte pointer to name

K$open		equ	0FFC0h		; open a logical file (after setlfs
					; and setnam)
					;O A = error # (1,2,4,5,6,240)

K$chkin		equ	0FFC6h		; open a channel for input
					;I X = logical file #
					;O A = errors #(0,3,5,6)

K$chkout	equ	0FFC9h		; open a channel for output
					;I X = logical file #
					;O A = error #(0,3,5,7)

K$clrchn	equ	0FFCCh		; clears ALL I/O channel 

K$chrin		equ	0FFCFh		; get a character from input channel
					;O A=input character 

K$chrout	equ	0FFD2h		; output a character to output channel
					;I A =output char

;GETIN		equ	0FFE4h

K$clall		equ	0FFE7h		; close ALL open logical files

K$close		equ	0FFC3h		; close a logical file
					;I A = logical channel # to be closed
					;O A = error #(0,240)

RESET		equ	0FFFCh

	PAGE
;
	org	bios8502
;
; **** THIS IS THE COMMAND LOOP ****
;
start:
  if	use$fast
	@ldx	sys$speed	;-K  get desired system speed
	@stx	vic$speed	;-K  set system speed
  endif
	@ldx	-1,#		;-K
	@txs			;-K set the stack to the top
	@JSR	VICIO		;-K  go find and do requested operation
bios$exit:
	@sei			;?K  DISABLE INTERRUPTS
	@ldx	3eh,#		;?K  set up Z80 memory map as required
	@stx	force$map	;?K
	@ldx	82h,#		;-K
	@stx	CIA1+int$ctrl	;-K  turn on CIA timer B interrupts
  if	use$fast
	@ldx	0,#		;-K  get value for 1 MHz mode (slow)
	@stx	vic$speed	;-K  set system speed
  endif
	@jmp	enable$z80+6	;-K

	PAGE
;
;
;
iotbl:
	dw	sys$reset	;-1 reset system (C128)
	dw	initilize	;0 initialize the 8502
	dw	READ		;1 Read a sector of data to sector buffer
	dw	WRITE		;2 Write a "     "   "   "    "      "
	dw	readf		;3 Set-up for fast read (154X only)
	dw	writef		;4 Set-up for fast write (154X only)
	dw	dsktst		;5 test for 154x and diskette type
	dw	query$dsk	;6 get disk characteristics
	dw	PRINT		;7 print data character
	dw	FORMAT		;8 format disk as 1541 disk
	dw	user$fun	;9 vector to user code (L=viccount,H=vicdata) 
	dw	ram$dsk$rd	;10 RAM disk read
	dw	ram$dsk$wr	;11 RAM disk write


NUMCMD		equ	($-IOTBL)/2	; NUMBER OF COMMANDS
iotbl$low	equ	low(iotbl)


;
;
;
sys$reset:			;**CMD ENTRY**
	@jsr	en$kernal	;-K
	@JMP	(RESET)		;+K
;
;
;
user$fun:			;**CMD ENTRY**
	@jmp	(vic$count)	;-K

	page
;
; **** IO COMMAND DISPATCH ROUTINE ****
;
VICIO:
	@lda	vic$cmd		;-K  get the command
	@cmp	NUMCMD,#	;-K  is this a valid command
	@bcs	bad$command	;-K  no, exit without doing anything
				;-K  yes, get vector to it
	@cld			;-K  clear to binary mode
	@asl	a		;-K  A=2*CMD (carry cleared)
	@clc			;-K
	@adc	iotbl$low+2,#	;-K  add to vector table start address
	@sta	VICIO2+1	;-K  modify the JMP instructions ind adr
VICIO2:
	@jmp	(IOTBL)		;-K  this is the ind adr that
				   ; is modified above
;
;
;
input$byte:
	@sei
	@lda	d2pra
	@eor	clk$bit,#
	@sta	d2pra
;
	@lda	8,#
in$1:
	@bit	d1icr
	@beq	in$1
	@lda	d1sdr
bad$command:
	@RTS			;-K

	page
;
;	initialize the 8502
;
initilize:			;**CMD ENTRY**
	@ldx	low(irqs),#		;-K
	@ldy	high(irqs),#		;-K
	@stx	314h			;-K  IRQ vector
	@sty	315h			;-K
	@stx	316h			;-K  BRK vector
	@sty	317h			;-K
	@stx	318h			;-K  NMI vector
	@sty	319h			;-K

	@jsr	en$kernal		;-K
	@lda	0fffeh			;+K
	@sta	0fffeh			;+K  write to RAM under ROM
	@lda	0ffffh			;+K
	@sta	0ffffh			;+K

	@lda	6,#			;+K
	@sta	CIA2+data$dir$b		;+K setup user port for RS232

	@lda	pal$nts			;+K -1=50Hz(PAL) 0=60Hz(NTSC)
	@sta	sys$freq		;+K
	@jmp	K$clall			;+K  close all open files

	PAGE
;
; **** DISK SECTOR READ ****
;
READ:				;**CMD ENTRY**
	@JSR	set$drv		;-K
	@jsr	en$kernal	;+K
	@ldx	datchn		;+K
	@jsr	K$chkin		;+K
	@bcs	disk$changed	;+K
	@jsr	K$clrchn	;+K  clear the input channel for now

	@LDA	'1',#		;+K  read command
	@JSR	setup		;+K  send it
	@JSR	CKINDT		;+K
	@LDX	0,#		;+K
;
READ1:
	@JSR	K$chrin		;+K  get a byte from the KERNAL
	@STA	@BUFFER,X	;+K  save it in the buffer
	@INX			;+K  advance the buffer pointer
	@BNE	READ1		;+K  loop back if not past buf end
	@jmp	K$clrchn	;+K  CLEAR CHANNEL
;
;
disk$changed:
	@lda	0bh,#		;?K  disk changed error code
	@sta	vic$data	;?K
	@jmp	en$K$open	;?K

	page
;
; **** DISK SECTOR WRITE ****
;
WRITE:				;**CMD ENTRY**
	@jsr	set$drv		;-K
	@jsr	ckotcm		;-K
	@LDY	setpnt$lng,#	;+K
;
WRITE0:
	@LDA	SETPNT,X	;+K
	@JSR	K$chrout	;+K
	@INX			;+K
	@DEY			;+K
	@BNE	WRITE0		;+K

	@JSR	K$clrchn	;+K
	@JSR	CKINCM		;+K
	@BNE	WRITE2		;+K

	@JSR	K$clrchn	;+K
	@JSR	CKOTDT		;+K
	@LDX	0,#		;+K
;
WRITE1:
        @sei 			;+K  disable interrupts
	@ldy	3fh,#		;+K  enable all RAM in bank 0
	@sty	force$map	;+K
	@LDA	@BUFFER,X	;-K
	@ldy	0,#		;-K  re-enable kernal
	@sty	force$map	;-K
	@JSR	K$chrout	;+K  write buffer character
	@INX			;+K
	@BNE	WRITE1		;+K  write all 256 bytes of buffer

	@JSR	K$clrchn	;+K  clear the channel
	@LDA	'2',#		;+K  write command
	@JMP	setup		;+K
;
WRITE2:
	@lda	0ffh,#		;+K
	@sta	vic$data	;+K  writes thru ROM to RAM
	@jmp	opencm		;+K

	page
;
;	Set-up for fast disk write
;
writef:				;**CMD ENTRY**
	@lda	2,#		;-K 2=read command
	@skip2			;-K
;
;	Set-up for fast disk read
;
readf:				;**CMD ENTRY**
	@lda	0,#		;-K 0=read command
	@sta	f$cmd		;-K
	@lda	0,#		;-K
	@sta	vic$data	;-K
	@jsr	set$drv$f	;-K
	@ldy	f$cmd$lng,#	;-K  command set above rd/wr
	@jsr	send$fast	;-K
	@jmp	clk$hi		;+K

	page
;
;	test the format of the disk return code to CP/M
;	telling the disk type. Also test for FAST disk drive.
;
dsktst:				;**CMD ENTRY**
	@lda	vic$drv		;-K
	@eor	0ffh,#		;-K
	@and	fast		;-K
	@sta	fast		;-K  clear fast indicator bit for current drive

	@jsr	set$and$open	;-K  set drv close and reopen the channel

	@ldx	0,#		;+K  delay to allow drive to reset status
tst$delay:
	@nop			;+K
	@nop			;+K
	@dex			;+K
	@bne	tst$delay	;+K

	@ldy	inq$cmd$lng,#	;+K
	@ldx	inq$cmd,#	;+K
	@jsr	send$fast$cmd	;+K

	@jsr	input$byte	;+K
	@sta	vic$data	;+K
	@jsr	clk$hi		;+K
	@sty	io$0		;+K  2/24
	@lda	vic$drv		;-K
	@ora	fast		;-K  set current drive as fast
	@sta	fast		;-K
	@rts			;-K

	page
;
;
;
query$dsk:				;**CMD ENTRY**
	@jsr	set$drv$f		;-K  will query track set by user
	@ldy	query$cmd$lng,#		;-K  command length is 4
	@ldx	query$cmd,#		;-K
	@jsr	send$fast$cmd		;-K
	@jsr	input$byte		;+K
	@sta	vic$data		;+K
	@bpl	clk$hi			;+K  exit if not MFM
	@and	0eh,#			;+K  test for error
	@bne	clk$hi			;+K  exit if error
	@jsr	input$byte		;+K  read offset sectors status byte
	@sta	@buffer			;+K
	@and	0eh,#			;+K  test for error
	@bne	clk$hi			;+K  exit if error 
	@tax				;+K get a zero in X
	@ldy	5,#			;+K five info bytes are sent back
query$loop:
	@inx
	@jsr	input$byte
	@sta	@buffer,X
	@dey
	@bne	query$loop

clk$hi:
	@lda	d2pra			;+K set clock bit HIGH
	@and	0ffh-clkbit,#
	@sta	d2pra
	@rts

	PAGE
;
; **** PRINTER OUTPUT ****
;
;	this routine will support two printers
;	the device number is passed in vic$drv (4,5)
;	secondary address in vic$trk
;	the logical file number is equal to the device #
;	if VIC$count=0 then output character in VIC$data
;	if VIC$count<>0 then output characters pointered to by @buffer
;
PRINT:				;**CMD ENTRY**
	@lda	vic$drv		;-K
	@sta	prtno		;-K
	@lda	vic$trk		;-K
;;	@sta	second$adr	;-K  this line should be deleted and one
	@cmp	second$adr	;-K  ..below used.
	@sta	second$adr	;-K  save secondary adr
	@bne	reopen$prt	;-K

	@jsr	en$kernal	;-K
print$cont:
	@ldx	prtno		;+K
	@JSR	K$chkout	;+K
	@BCS	PERR0		;+K  PRINT ERROR IF CARRY SET

	@sty	io$0		;+K  2/24
	@ldx	vic$count	;-K  
	@bne	print$buffer	;-K
	@LDA	vic$data	;-K  GET CHARACTER
	@sta	io$0		;-K
	@JSR	K$chrout	;+K  AND PRINT IT
	@JMP	K$clrchn	;+K  CLEAR CHANNEL

print$buffer:
	@stx	temp$byte	;-K
	@lda	@buffer		;-K
	@sta	adr$1		;-K
	@lda	@buffer+1	;-K
	@sta	adr$1+1		;-K
	@ldy	0,#		;-K
	@ldx	0,#		;-K

print$buf$loop:
	@sta	bank$0		;?K	enable RAM bank 0 (no I/O)
	@lda	(adr$1),y	;rK
	@stx	force$map	;rK
	@jsr	K$chrout	;+K
	@iny			;+K
	@dec	temp$byte	;+K
	@bne	print$buf$loop	;+K
	@jmp	K$clrchn	;+K

;
;
PERR0:
	@CMP	3,#		;+K  FILE NOT OPEN?
	@BNE	PERR1		;+K  BRANCH IF NO
reopen$prt:
	@JSR	OPNPRT		;?K  OPEN PRINTER CHANNEL
	@BCC	print$cont	;+K  IF CARRY CLEAR, OK TO PRINT
PERR1:	@LDA	255,#		;+K  NO DEVICE PRESENT
	@STA	vic$data	;+K  FLAG BAD ATTEMPT writes to ram under ROM
PRTST:	@RTS			;+K

	PAGE
;
; **** FORMAT DISK ROUTINE ****
;
FORMAT:				;**CMD ENTRY**
	@jsr	set$drv$num	;-K
	@lda	fast		;-K
	@and	vicdrv		;-K
	@bne	format$fast	;-K

	@JSR	CKOTCM		;-K  returns X=0
	@LDY	fmtcmd$lng,#	;+K
FMT1:	@LDA	FMTCMD,X	;+K
	@JSR	K$chrout	;+K
	@INX			;+K
	@DEY			;+K
	@BNE	FMT1		;+K
	@JSR	K$clrchn	;+K
fmt2:	@JSR	CKINCM		;+K check for errors
	@BEQ	setup3		;+K no errors, return good status
	@BNE	setup5		;+K error return error status

format$fast:
	@ldx	@buffer		;-K get command length
fast$F:				;-K
	@lda	@buffer+1-1,x	;-K
	@sta	F$cmd-1,x	;-K
	@dex			;-K transfer command tail from buffer+1
	@bne	fast$F
	@ldy	@buffer		;-K
	@iny			;-K
	@iny			;-K  count is tail length plus 2
	@ldx	F$cmd		;-K
	@jsr	send$fast$cmd	;-K
	@jmp	fmt2		;+K

	PAGE
;
;
;
ram$dsk$rd:			; RAM disk read
	@ldx	81h,#			;-K
	@skip$2				;-K
;
;
;
ram$dsk$wr:			; RAM disk write
	@ldx	80h,#			;-K
  if	use$fast
	@lda	0,#			;-K  0=slow (1 MHz)
	@sta	vic$speed		;-K  set to slow mode
  endif
	@lda	3Fh,#
	@stx	RM$command		;-K  give command to RAM DISK
 	@sta	force$map		;    remove I/O area
	@rts				;-K


	PAGE
;
;
;
setup:
	@STA	DSKCMD+1	;?K
	@LDA	2,#		;?K  RETRY COUNT
	@STA	vic$data	;?K  writes to RAM under ROM

	@JSR	CKOTCM		;?K  returns X=0
	@LDY	dskcmd$lng,#	;+K
setup2:
	@LDA	DSKCMD,X	;+K
	@JSR	K$chrout	;+K
	@INX			;+K
	@DEY			;+K
	@BNE	setup2		;+K

	@JSR	K$clrchn	;+K
	@JSR	CKINCM		;+K
	@BEQ	setup3		;+K

	@sty	io$0		;+K  2/24
	@DEC	vic$data	;-K
	@BEQ	setup5		;-K

	@jmp	disk$changed	;-k
;
;
setup5:
	@LDA	0dh,#		;?K  normal read/write error flag
	@skip2			;?K  ALWAYS
;
;
;
setup3:
	@lda	0,#		;?K  get data good flag
setup4:
	@STA	vic$data	;?K  writes to RAM under ROM	
	@jsr	en$kernal	;?K
	@JMP	K$clrchn	;+K

	page
;
;
;
send$fast$cmd:
	@jsr	set$cmd		;?K  unit # must have been set already
send$fast:
	@ldx	0,#		;?K
	@stx	force$map	;?K  enable the kernal
	@ldx	cmdchn		;+K
	@jsr	K$chkout	;+K
	@bcs	chan$error	;+K
	@ldx	0,#		;+K
sendf:
	@lda	f$cmd$buf,x	;+K
	@jsr	K$chrout	;+K
	@inx			;+K
	@dey			;+K
	@bne	sendf		;+K
	@jsr	K$clrchn	;+K
	@bit	serial		;+K
	@bvc	not$fast	;+K
	@bit	d1icr		;+K  clear interrupts from chip
	@rts			;+K

chan$error:
	@lda	0dh,#		;+K  get error code
	@skip2			;+K
not$fast:
	@lda	0ch,#		;+K  get error code
	@sta	vic$data	;+K
	@jsr	clk$hi		;+K
	@jmp	bios$exit	;+K
;
;
;
set$cmd:
	@lda	dskcmd+5	;?K check lsb of unit #
	@ror	a		;?K get lsb to carry bit
	@bcc	unit$0		;?K
	@inx			;?K  make command for unit 1
unit$0:
	@stx	F$cmd		;?K
	@rts

	page
;
;	........not tested........
;
;rd$buff:
;	@sei			; disable interrupts
;	@lda	vic$data
;	@sta	adr$1+1		; save hi part of address
;	@lda	0,#
;	@sta	adr$1		; save low part of address
;	@tax			; get a zero for both indexes
;	@tay
;
;rd$buf$1:
;	@lda	(adr$1),y	
;	@sta	@buffer,x
;	@inx
;	@iny
;	@bne	rd$buf$1
;	@rts

	PAGE
;
;
;
set$drv:
	@lda	vic$trk		;-K
	@jsr	binasc		;-K
	@stx	dskcmd+7	;-K
	@sta	dskcmd+8	;-K

	@lda	vic$sect	;-K
	@bmi	no$side$1	;-K
	@jsr	binasc		;-K
	@stx	dskcmd+10	;-K
	@sta	dskcmd+11	;-K
	@jmp	set$drv$num	;-K

no$side$1:
	@lda	04h,#		;-K
	@sta	vic$data	;-K
	@jmp	bios$exit	;-K

;
;
;
set$drv$f:
	@lda	vic$count	;-K
	@sta	f$rd$count	;-K

	@lda	vic$trk		;-K
	@sta	f$rd$trk

	@lda	vic$sect	;-K
	@bpl	side$0		;-K
	@tax			;-K
	@lda	f$cmd		;-K
	@ora	10h,#		;-K
	@sta	f$cmd		;-K
	@txa			;-K
	@and	7fh,#		;-K
side$0:
	@sta	f$rd$sect	;-K

	page
;
;		 VIC$DRV		       dev,dat,cmd
;		00000001	device  #8-0	 8,11,15
;		00000010	device  #9-0	 9,12,16
;		00000100	device #10-0	10,13,17
;		00001000	device #11-0	11,14,18
;		10000001	device  #8-1	 8,11,15
;		10000010	device	#9-1	 9,12,16
;		10000100	device #10-1	10,13,17
;		10001000	device #11-1	11,14,18
;
set$drv$num:
	@ldy	8-1,#		;-K start as drive 8
	@ldx	'0',#		;-K ..unit 0
	@lda	vic$drv		;-K get requested drv#
	@bpl	unit$nu$0
	@inx			;-K make unit 1
unit$nu$0:
	@iny			;-K add one to the drive #
	@lsr	a		;-K is drive number correct?
	@bcc	unit$nu$0	;-K no, loop back

	@stx	dskcmd+5	;-K save unit# to disk cmd string
	@stx	fmtcmd+1	;-K save unit# to format cmd string
	@txa
	@ror	a		;-K get lsb to carry bit
	@lda	F$cmd
	@and	0feh,#
	@adc	0,#		; set the lsb if carry set (carry cleared)
	@sta	F$cmd
	@tya 			;-K get device # to A
	@sta	devno		;-K save device #
	@adc	3,#		;-K make the data chan# (carry cleared above)
	@sta	datchn		;-K save data chan#
	@adc	4,#		;-K make the cmd chan#
	@sta	cmdchn		;-K save cmd chan#
	@lda	serial		;-K
	@and	0bfh,#		;-K
	@sta	serial		;-K  clear the fast serial indicator
	@rts			;-K

	page

;
; **** CONVERT BINARY TO ASCII ****
;
BINASC:
	@CLD			;?K
	@LDX	'0',#		;?K
	@SEC			;?K

BA0:
	@SBC	10,#		;?K
	@BCC	BA1		;?K

	@INX			;?K
	@BCS	BA0		;?K

BA1:
	@ADC	3Ah,#		;?K
	@RTS			;?K

	PAGE
;
; **** OPEN DISK COMMAND CHANNEL ****
;
set$and$open:
	@jsr	set$drv$num	;-K
en$K$open:
	@jsr	en$kernal	;-K
opencm:
	@LDA	CMDCHN		;+K
	@clc			;+K  clear the carry to force true closing 
	@JSR	K$close		;+K

	@LDA	CMDCHN		;+K
	@LDX	DEVNO		;+K
	@LDY	15,#		;+K
	@JSR	K$setlfs	;+K

	@lda	0,#		;+K  bank (C128 type) for load and store 
	@sta	F$stat		;+K  write status byte value = 0
	@tax			;+K  file name bank (C128 type bank#)
	@jsr	K$setbnk	;+K

	@ldx	write$stat,#	;+K
	@jsr	set$cmd		;+K

	@lda	4,#		;+K write status command lenght
	@ldx	low(f$cmd$buf),#	;+K
	@ldy	high(f$cmd$buf),#	;+K
	@JSR	K$setnam		;+K

	@JSR	K$open		;+K
	@bcs	misdsk

	@JSR	K$readst
	@ROL	A		;+K  GET MSB TO CARRY
	@BCS	MISDSK		;+K  DEVICE MISSING IF CARRY SET

	@bit	serial		;+K  test for fast device
	@bvs	no$dt$open	;+K  do not open data channel if fast
;
; **** OPEN DISK DATA CHANNEL ****
;
OPENDT:
	@LDA	DATCHN		;+K
	@clc			;+K  forces true closing of channel
	@JSR	K$close		;+K
	@LDA	DATCHN		;+K
	@LDX	DEVNO		;+K
	@LDY	8,#		;+K
	@JSR	K$setlfs	;+K
	@lda	0,#		;+K  bank (C128 type) for load and store 
	@tax			;+K  file name bank (C128 type bank#)
	@jsr	K$setbnk	;+K
	@LDA	1,#		;+K
	@LDX	low(POUND),#	;+K
	@LDY	high(POUND),#	;+K
	@JSR	K$setnam	;+K
	@jsr	K$open		;+K
	@bcs	misdsk
no$dt$open:
	@rts

	page
;
;
;  * DEVICE MISSING, CLEAN UP ERROR *
;
MISDSK:
	@LDA	0fh,#		;+K  SET ERROR CODE for missing drive
	@STA	vic$data	;+K  writes to RAM under ROM
	@LDA	CMDCHN		;+K  K$close CHANNEL
	@clc			;+K  force true closing of channel
	@JSR	K$close		;+K
	@JMP	bios$exit	;+K

	PAGE
;
; **** SELF CORRECTING CHECK IO ROUTINES ****
;
CKICM:
	@JSR	OPENCM		;+K

CKINCM:
	@LDX	CMDCHN		;+K
	@JSR	K$chkin		;+K
	@BCS	CKICM		;+K

	@JSR	K$chrin		;+K
	@CMP	'0',#		;+K
	@RTS			;+K
;
;
;
CKIDT:
	@JSR	OPENDT		;+K
CKINDT:
	@LDX	DATCHN		;+K
	@JSR	K$chkin		;+K
	@BCS	CKIDT		;+K

	@RTS			;+K
;
;
;
CKODT:
	@JSR	OPENDT		;+K
CKOTDT:
	@LDX	DATCHN		;+K
	@JSR	K$chkout	;+K
	@BCS	CKODT		;+K

	@RTS			;+K
;
;
;
CKOCM:
	@jsr	OPENCM		;+K
CKOTCM:
	@jsr	en$kernal	;?K
	@LDX	CMDCHN		;+K
	@JSR	K$chkout	;+K
	@BCS	CKOCM		;+K

	@LDX	0,#		;+K
	@RTS			;+K

	PAGE
;
; **** OPEN PRINTER CHANNEL ****
;
opnprt:
	@jsr	en$kernal	;-K
	@lda	prtno		;+K
	@clc			;+K
	@JSR	K$close		;+K

	@lda	prtno		;+K
	@TAX			;+K  LDX #4 (or #5)
	@ldy	second$adr	;+K secondary adr passed in vic$trk (normaly=7)
	@JSR	K$setlfs	;+K
	@LDA	0,#		;+K
	@JSR	K$setnam	;+K
	@lda	0,#		;+K  bank (C128 type) for load and store 
	@tax			;+K  file name bank (C128 type bank#)
	@jsr	K$setbnk	;+K
	@JMP	K$open		;+K

	page
;
;	handle all interrupts in BIOS 8502 (throw them away)
;
irqs:
	@lda	CIA$1+int$ctrl
	@lda	CIA$2+int$ctrl
	@lda	0fh,#
	@sta	VIC+25

	@pla
	@sta	force$map
	@pla
	@tay
	@pla
	@tax
	@pla
	@rti

;
;
;
en$kernal:
	@ldy	0,#		;?K
	@sty	force$map	;?K
	@rts			;+K

	page
;
;
;
DSKCMD:		db	'U1:8 0 tt ss',CR
dskcmd$lng	equ	$-dskcmd

POUND:		db	'#'

FMTCMD:		db	'N0:CP/M DISK,65',CR
fmtcmd$lng	equ	 $-FMTCMD

SETPNT:		db	'B-P 8 0',CR
setpnt$lng	equ	$-setpnt
;
;	fast command buffer
;
f$cmd$buf:	db	'U0'	; not set
f$cmd:		db	0	; byte 3
F$stat:
f$rd$trk:	db	1	; byte 4
f$rd$sect:	db	0	; byte 5
f$rd$count:	db	1	; byte 6
		db	0	; byte 7
		db	0	; byte 8
		db	0	; byte 9
		db	0	; byte 10
		db	0	; byte 11

f$cmd$lng	equ	6		; U0+cmd+track+sector+#sectors

write$stat	equ	01001100b
write$stat$lng	equ	4		; U0+cmd+(status to write)

inq$cmd:	equ	00000100b
inq$cmd$lng	equ	3		; U0+cmd

query$cmd:	equ	10001010b
query$cmd$lng	equ	4		; U0+cmd+(track offset)


