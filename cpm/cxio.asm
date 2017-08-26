



;
	title	'C128 BIOS, main I/O and sys functions     28 Apr 86'

;
;	This module contains CXIO,CXINIT,CXMOVE and CXTIME.
;
	maclib	cpm3

	maclib	z80

	maclib	cxequ

	maclib	modebaud


	public	?init,?ldccp,?rlccp

	public	?user,?di$int

	extrn	?sysint

bdos	equ	5	

	extrn	@civec,@covec,@aivec,@aovec,@lovec
	extrn 	?bnksl

	public	?cinit,?ci,?co,?cist,?cost
	public	@ctbl
	extrn	?kyscn

; Utility routines in standard BIOS
	extrn	?wboot		; warm boot vector
        extrn   ?pmsg           ; print message @<HL> up to 00
                                ; saves <BC> & <DE>
        extrn   ?pdec           ; print binary number in <A> from 0 to 99.
	extrn	?pderr		; print BIOS disk error header
	extrn	?conin,?cono	; con in and out
	extrn	?const		; get console status

	extrn	@hour,@min,@sec,@date,?bnksl
	public	?time

	page
;
;	keyboard scanning routine 
;
	extrn	?get$key,?int$cia
	extrn	Fx$V$tbl
;
;	links to 80 column display
;
	extrn	?out80,?int80
	extrn	?out40,?int40

	extrn	?pt$i$1101,?pt$o$1,?pt$o$2
	extrn	?convt
;	extrn	?pt$s$1101

;
;	bios8502 function routines
;
	public	?fun65

;
;
;
	public	?intbd
	extrn	?int65,?in65,?ins65,?out65,?outs65

;	cseg
;trace:
;	xthl			; save hl on stack
;	push	psw
;	call	?pmsg		; DE and BC saved by ?pmsg
;	pop	psw
;	xthl
;	ret
;
;	CSEG
;disp$A:
;	push	psw		;;;test
;	ani	0fh		;;;test
;	adi	90h		;;;test
;	daa			;;;test
;	aci	40h		;;;test
;	daa			;;;test
;	sta	low$test	;;;test
;	pop	psw		;;;test
;	rar			;;;test
;	rar			;;;test
;	rar			;;;test
;	rar			;;;test
;	ani	0fh		;;;test
;	adi	90h		;;;test
;	daa			;;;test
;	aci	40h		;;;test
;	daa			;;;test
;	sta	hi$test		;;;test
;	call	trace		;;;test
;hi$test:			;;;test
;	db	31		;;;test
;low$test:			;;;test
;	db	31		;;;test
;	db	' '		;;;test
;	db	0		;;;test
;	ret			;;;test
;
	page

	DSEG
?fun65:
	sta	vic$cmd			; save the command passed in A
   if	not use$6551
fun$di$wait:
	lda	RS232$status
	ani	11000010b		; char to Xmit, Xmiting or receiving ?
	jrnz	fun$di$wait		; yes, wait for int to clean up
   endif
	di
	lda	force$map		; get current MMU configuration
	push	psw			; save it
	sta	io$0			; make I/O 0 current

	lxi	d,1			; D=0,  E=1
   if	use$fast
	lxi	b,VIC$speed
	inp	a
	sta	sys$speed
	outp	d			; set slow mode (1 2 MHz Z80)
   endif
	lxi	b,page$1$h
	outp	d
	dcr	c
	outp	e			; page 1, 0-1
	dcr	c
	outp	d
	dcr	c
	outp	d			; page 0, 0-0
	call	enable$6502+6		; go run the 8502
	mvi	c,low(page$1$h)
	outp	e
	dcr	c
	outp	e			; page 1, 1-1
	dcr	c
	outp	e
	dcr	c
	outp	d			; page 0, 1-0
   if	use$fast
	lxi	b,VIC$speed
	lda	sys$speed		; get desired system speed
	outp	a			; set speed (2 or 4 MHz Z80)
   endif
	pop	psw			; recover the MMU config.
	sta	force$map		; restore it
	ei				; turn interrupts back on
	lda	vic$data		; get command results
	ora	a			; set the zero flag if A=0
	ret

?di$int:
   if	not use$6551
	push	psw
di$int$1:
	lda	RS232$status		; character to Xmit or currently
	ani	11000010b		; ..transmitting or receiving ?
	jrnz	di$int$1		; yes, wait for int to clean up
	pop	psw
   endif
	di
	ret

	page
;
;	set up the MMU for CP/M Plus
;
	DSEG			; init done from banked memory
?init:
	mvi	a,3eh			; force MMU into I/O space
	sta	force$map		;
	lxi	h,mmu$table+11-1	; table of 11 values
	lxi	b,mmu$start+11-1	; to to MMU registers
	mvi	d,11			; move all 11 bytes to the MMU

init$mmu$loop:
	mov	a,m
	outp	a
	dcx	h
	dcx	b
	dcr	d
	jrnz	init$mmu$loop

	mvi	a,1			; enable track and sector status
	sta	stat$enable		; on the status line
;	mvi	a,1			; no parity, 8 bits, 1 stop bit
	sta	XxD$config
;
   if	use$6551
	lxi	h,int$6551
   else
	lxi	h,usart
   endif
	shld	usart$adr

	lxi	h,?convt
	shld	prt$conv$1
	shld	prt$conv$2

	lxi	h,Fx$V$tbl
	shld	key$FX$function
;
; install I/O assignments
;
	lxi	h,4000h+2000h 		; 80 and 40 column drivers
	shld	@covec
	mvi	h,80h
	shld	@civec			; assign console input to keys
	mvi	h,10h
	shld	@lovec			; assign printer to LPT:
	mvi	h,00h
	shld	@aivec
	shld	@aovec			; assign rdr/pun port

	page
;
; print sign on message
;
	call	prt$msg			; print signon message
	db	'Z'-'@'			; initialize screen pointers
	db	esc,esc,esc
	db	purple+50h		; set character color
	db	esc,esc,esc
	db	black+60h		; set background (BG) color
	db	esc,esc,esc
	db	brown+70h		; set border color
	db	'Z'-'@'			; home and clear screen (to BG color)

	db	lf,lf,lf
    if	use$fast
	db	'Fast '
    endif

    if	use$6551
	db	'/w 6551 '
    endif

	db	'CP/M 3.0'
    if	not banked
	db	' Non-Banked'
    endif
	db	' On the Commodore 128 '
	date
	warning
	db	cr,lf
	db	'          ',0

;
;	set CONOUT driver to correct screen
;
	lxi	h,4000h			; 80 column screen vector	
	call	read$d505
	ral
	jrnc	set$screen
	mvi	a,'4'
	sta	screen$num
	mvi	h,20h			; 40 column screen vector

set$screen:
	call	prt$msg			; HL saved
screen$num:
	db	'80 column display',cr,lf,lf,lf,lf,0
	shld	@covec			; assign console output to CRT: (40/80)

	page

;
;
	mvi	a,-1			; set block move to NORMAL mode
	sta	source$bnk
;
;	install mode 2 page vectors
;
	mvi	a,JMP
	sta	INT$vector		; install a JMP at vector location
	lxi	h,?sysint
	shld	INT$vector+1		; install int$handler adr
;
; A software fix is  required for the lack of hardware to force the
; LSB of the INT vector to 0. If the bus floats INT VECT could be
; read as 0FFh; thus ADRh=I (I=0FCh) ADRl=FF for first read, and
; ADRh=I+1 ADRl=00 for second, to ensure that control is retained
; 0FD00h will also have FDh in it.
;
	lxi	h,int$block		; FC00h
	lxi	d,int$block+1		; FC01h
	lxi	b,256-1+1		; interrupt pointer block
	mvi	m,INT$vector/256	; high and low are equal (FD)
	ldir
	mvi	a,INT$block/256
	stai				; set interrupt page pointer
	im2				; enable mode 2 interrupts

	page
;
;
	mvi	a,vicinit		; null command just to setup BIOS8502
	call	?fun65
;
;
;
	lda	sys$freq		; 0=60Hz 0FFh=50Hz
	ani	80h			; 0=60Hz 080h=50Hz
	mov	l,a			; save in L
	lxi	b,cia$1+0eh		; point to CRA
	inp	a			; get old config
	ani	7fh			; clear freq bit
	ora	l			; add in new freq bit
	outp	a			; set new config

	mvi	c,8			; start RTC
	outp	a

	lxi	h,date$hex
	shld	@date			; set date to system data

;
;	setup the sound variables
;
	lhld	key$tbl
	lxi	d,58*4
	dad	d
	mov	e,m
	inx	h
	mov	d,m
	inx	h
	xchg
	shld	sound1			; H=SID reg 24, L=SID reg 5
	xchg
	mov	e,m
	inx	h
	mov	d,m
	xchg
	shld	sound2			; H=SID reg 6, L=SID reg 1
	lxi	h,9
	dad	d
	mov	e,m
	inx	h
	mov	d,m
	xchg
	shld	sound3			; H=SID reg 4 then L=SID reg 4 
;
;	set-up key click sound registers
;
	lxi	b,sid+7
	lxi	h,0040h
	outp	l			; (sid+7)=40h
	inr	c
	outp	l			; (sid+8)=40h
	mvi	c,low(sid+12)
	outp	h			; (sid+12)=0  Attack=2ms, Decay=6ms
	inr	c
	outp	h			; (sid+13)=0  Sustain=0,  Release=6ms
	mvi	a,6
	sta	tick$vol		; set keyclick volumn level
;
;	set up interrupts for key scan (not software usart)
;
   if	use$6551
	lxi	d,2273			; int at 150 BAUD rate
	lxi	b,CIA1+timer$b$low	;
	outp	e			;
	inr	c			; point to timer$b$high
	outp	d			;

	mvi	a,11h			;
	mvi	c,CIA$ctrl$b		; turn on timer B
	outp	a			;

	lxi	b,CIA2+data$b		; setup user port for RS232
	inp	a			; get old data
	ori	6			; set CTS and DTR
	outp	a			; update it
   endif
 	ret


mmu$table:
	mmu$tbl$M

	page
;
;
;
	CSEG
prt$msg:
	xthl
	call	?pmsg
	xthl
	ret


;
;	placed in common memory to keep IO from stepping on this code
;		always called from bank 0
;
	CSEG
read$d505:
	sta	io$0			; enable MMU (not RAM)
	lxi	b,0d505h
	inp	a			; read 40/80 column screen
	sta	bank$0			; re-enable RAM
	ret

	page
;
;
;
	DSEG
   if	not use$6551
init$RS232:
	di

	xra	a
	sta	RS232$status
	lxi	h,RxD$buf$count		; clear the count
	mvi	m,0
	inr	l			; point to RxD$buf$put
	mvi	m,low(RxD$buffer)
	inr	l			; point to RxD$buf$get
	mvi	m,low(RxD$buffer)

	lxi	h,NTSC$baud$table
	lda	sys$freq
	ora	a
	jrz	use$NTSC
	lxi	h,PAL$baud$table
use$NTSC:
	lda	RS232$baud
	cpi	baud$1200		; baud rate less then 1200 baud
	jrc	baud$ok			; yes, go set it
	mvi	a,baud$1200		; no, 1200 baud is the max
	sta	RS232$baud		; (change to 1200 baud)

baud$ok:
	mov	e,a
	mvi	d,0
	dad	d			; +1X
	dad	d			; +1X
	dad	d			; +1X = +3X
	mov	e,m
	inx	h
	mov	d,m
	inx	h			;
	mov	a,m			; get rate #
	sta	int$rate		;
	lxi	b,CIA1+timer$b$low	;
	outp	e			;
	inr	c			; point to timer$b$high
	outp	d			;

	mvi	a,11h			;
	mvi	c,CIA$ctrl$b		; turn on timer B
	outp	a			;

	lxi	b,CIA2+data$b		; setup user port for RS232
	inp	a			; get old data
	ori	6			; set CTS and DTR
	outp	a			; update it
	ei
	ret

	page
;
;	NTSC rates (1.02273 MHz)
;
NTSC$baud$table:
	dw	6818			; no baud rate	 (6666.47)
	db	1
	dw	6818			; 50	6666.7us (6666.47)
	db	1
	dw	4545			; 75	4444.4us (4443.99)
	db	1
	dw	3099			; 110	3030.3us (3030.13)
	db	1
	dw	2544			; 134	2487.6us (2487.46)
	db	1
	dw	2273			; 150	2222.2us (2222.48)
	db	2
	dw	1136			; 300	1111.1us (1110.75)
	db	3
	dw	568			; 600	 555.6us ( 555.38)
	db	6
	dw	284			; 1200	 277.8us ( 277.69)
	db	12

;
;	PAL rates (0.98525 MHz)
;
PAL$baud$table:
	dw	6568			; no baud rate	  (6666.32)
	db	1
	dw	6568			; 50	 6666.7us (6666.32)
	db	1
	dw	4379			; 75	 4444.4us (4444.56)
	db	1
	dw	2986			; 110	 3030.3us (3030.70)
	db	1
	dw	2451			; 134	 2487.6us (2487.69)
	db	1
	dw	2189			; 150	 2222.2us (2221.77)
	db	2
	dw	1095			; 300	 1111.1us (1111.39)  300*3
	db	3
	dw	547			; 600	  555.6us ( 555.19)  600*3
	db	6
	dw	274			; 1200    277.8us ( 278.10) 1200*3
	db	12

	page
;
;
;
out$RS232:
	call	out$st$RS232
	jrz	out$RS232
	mov	a,c
	sta	xmit$data		; get character to send in A
	lxi	h,RS232$status
	setb	7,m			; set Xmit request bit
	ret

;
;
;
out$st$RS232:
	lda	RS232$status
	ani	80h			; bit 8 set if busy
	xri	80h			; A cleared if busy (=80h if not)
	rz
	ori	0ffh			; A=ff if ready (not busy)
	ret

;
;
;
in$RS232:
	call	in$st$RS232
	jrz	in$RS232
	lda	recv$data
	lxi	h,RS232$status
	res	0,m
	ret

;
;
;
in$st$RS232:
	lda	RS232$status
	ani	1
	rz
	ori	0ffh			; set data ready (-1)
	ret
   endif
	page
;
;	this routine is used to provide the user with a method
;	of interfacing with low level system functions
;
	CSEG
;
;	input:
;		all registers except HL and A are passed to function
;
;	output:
;		all resisters from function are preserved
;
?user:
	shld	user$hl$temp
	xchg
	shld	de$temp			; save DE for called function

	mov	e,a			; place function number in E
	mvi	a,num$user$fun-1	; last legal function number

	call	vector			; function
usr$tb:	dw	read$mem$0		; 0
	dw	write$mem$0		; 1
	dw	?kyscn			; 2
	dw	do$rom$fun		; 3  (L=function #) 	
	dw	do$6502$fun		; 4  (L=function #)
	dw	read$d505		; 5  returns MMU reg in A
	dw	code$error		; not 0 to 5 ret version number in HL

num$user$fun	equ	($-usr$tb)/2

	page
;
;	address in DE is read and returned in C
;	A=0 if no error
;
	DSEG
read$mem$0:
	ldax	d			; read location addressed by DE
	mov	c,a			; value returned in C
	xra	a			; clear error flag
	ret

;
;	address in DE is written to with value in C
;	A=0 if no errors
;
write$mem$0:
	mvi	a,-1			; get error flag and 0ffh value
	cmp	d			; do not allow write from FF00 to FFFF
					;   this is 8502 space, MMU direct reg.
	rz
	mov	a,d
	cpi	10h			; do not allow write from 0000 to 0FFF
					;   this is ROM space
	mvi	a,-1			; get error flag
	rc				; return if 00h to 0fh
	mov	a,c
	stax	d
	xra	a			; clear error flag 
	ret

	page
;
;	This is the function code entry point for direct execution
;	of driver functions. If the MSB of the function number is
;	set, the 40 column driver is used; else the 80 column drive 
;	is used.
;
do$rom$fun:
	lhld	user$hl$temp		; get HL (L=fun #)

	mvi	a,7eh			; only allow even functions
	ana	l
	cpi	79h
	jrc	no$hl$req
	lhld	@dma			; HL will be passed in @dma by
	push	h			; ..the user
no$hl$req:
	mov	l,a
	rst	5			; call rom functon (RCALL) L=fun #	
	ret

;	mvi	a,7eh			; only allow even functions
;	ana	l
;	sta	no$hl$req+1
;	cpi	79h
;	jrc	no$hl$req
;	lhld	@dma			; HL will be passed in @dma by
;	push	h			; ..the user
;no$hl$req:
;	will be changed to RCALL xx   RET for next release (ROM FN 7A, 7C
;		and 7E will not function with current code, they expect
;		a return address on the stack
;
;	RJMP	5Eh			; unused function, real fun# installed
					; ..above

do$6502$fun:
	lhld	user$hl$temp
	mov	a,l
	jmp	?fun65
;
;
;
code$error:
	lxi	h,date$hex
	mvi	a,-1
	ret

	page
;
;
;
	CSEG
?rlccp:
	lxi	h,ccp$buffer
	lxi	b,0c80h

load$ccp:
	sta	bank$0
	mov	a,m
	sta	bank$1
	lxi	d,-ccp$buffer+100h
	dad	d
	mov	m,a
	lxi	d,ccp$buffer-100h+1
	dad	d
	dcx	b
	mov	a,b
	ora	c
	jrnz	load$ccp
	ret

	page
;
;
;
	CSEG
?ldccp:
	xra	a
	sta	ccp$fcb+15	; zero extent
	lxi	h,0
	shld	fcb$nr		; start at beginning of file
	lxi	d,ccp$fcb
	call	open		; open file containing CCP
	inr	a
	jrz	no$CCP		; error if no file...
	lxi	d,0100h
	call	setdma		; start of TPA
	lxi	d,128
	call	setmulti	; allow up to 16K bytes
	lxi	d,ccp$fcb
	call	read

	lxi	h,0100h
	lxi	b,0c80h
	lda	force$map
	push	psw

;
;
save$ccp:
	sta	bank$1
	mov	a,m
	sta	bank$0
	lxi	d,ccp$buffer-100h
	dad	d
	mov	m,a
	lxi	d,-ccp$buffer+100h+1
	dad	d
	dcx	b
	mov	a,b
	ora	c
	jrnz	save$ccp

	pop	psw
	sta	force$map
	ret

	page 
;
;	The following code does not work with the NEW MMU
;
;?ldccp:
;	xra	a
;	sta	ccp$fcb+15	; zero extent
;	lxi	h,0
;	shld	fcb$nr		; start at beginning of file
;	lxi	d,ccp$fcb
;	call	open		; open file containing CCP
;	inr	a
;
;;	trace	jz below should be jrz
;	jz	no$CCP		; error if no file...
;
;	lda	fcb$rc		; get the record count
;	sta	ccp$count	; save for later
;	lxi	d,0100h
;	call	setdma		; start of TPA
;	lxi	d,128
;	call	setmulti	; allow up to 16K bytes
;	lxi	d,ccp$fcb
;	call	read
;
;	lxi	d,1f0h		; point to buffer
;				; bank 1, page F0
;;	lxi	h,101h		; point to CCP (in TPA)
;				; bank 1, page 01
;	mov	h,d
;	mov	l,d
;	jr	save$ccp
;
;
;
;
;?rlccp:
;	lda	ccp$count	;
;	sui	30		; we can only save 30 records
;	jp	?ldccp
;
;	lxi	h,1F0h		; point to buffer
;				; bank 1, page F0
;;	lxi	d,101h		; point to TPA space
;				; bank 1, page 01
;	mov	d,h
;	mov	e,h
;
;save$ccp:
;	mvi	b,15		; number of pages in buffer
;ccp$move$loop:
;	push	h
;	push	d
;	push	b
;	call	do$move$0$to$1
;	pop	b
;	pop	d
;	pop	h
;	inx	h
;	inx	d
;	djnz	ccp$move$loop
;
;	ret
;
;
;do$move$0$to$1:
;	call	set$0$and$1
;	call	move$0$to$1
;	lxi	h,100h		; bank 1 page 0
;;	lxi	d,101h		; bank 1 page 1
;	mov	d,h
;	mov	e,h
;;
;;
;;
;set$0$and$1:
;	lda	force$map	; get current map
;	sta	io		; force to i/o in bank 0
;	lxi	b,page$0$l	; point to 1st page register
;	outp	l		; set page 0 low
;	inr	c
;	outp	h		; set page 0 high
;	inr	c
;	outp	e		; set page 1 low
;	inr	c
;	outp	d		; set page 1 high
;	sta	force$map
;	ret
;
;;
;;
;;
;move$0$to$1: 
;	lda	force$map
;	sta	bank$1		; force bank 1 memory
;	lxi	h,000h		; source
;	lxi	d,100h		; dest.
;;	lxi	b,100h
;	mov	b,d
;	mov	c,e		; count
;	ldir
;	sta	force$map
;	ret
;
	page
;
;
;
no$CCP:				; here if we couldn't find the file
	call	prtmsg		; report this...
	db	cr,lf,'BIOS Err on A: No CCP.COM file',0
	call	?conin		; get a response
	jr	?ldccp		; and try again

;
; CP/M BDOS Function Interfaces
;
	CSEG
open:
	mvi	c,15		; open file control block

	db	21h		; lxi h,(mvi c,26)
setdma:
	mvi	c,26		; set data transfer address

	db	21h		; lxi h,(mvi c,44)	
setmulti:
	mvi	c,44		; set record count

	db	21h		; lxi h,(mvi c,20)
read:
	mvi	c,20		; read records
	jmp	bdos

;			   12345678901
ccp$fcb		db	1,'CCP     COM',0,0,0
fcb$rc		db	0
		ds	16
fcb$nr		db	0,0,0


	page
;
;	CXIO.ASM and CXEM.ASM
;
;==========================================================
;		ROUITINE TO VECTOR TO HANDLER
;==========================================================
;	CP/M IO routines	b=device : c=output char : a=input char
;
	CSEG
;
;
;
?cinit:				; initialize usarts
	mov	b,c
	call	vector$io	; jump with table adr on stack
number$drivers:
	dw	?int$cia	; keys
	dw	?int80		; 80col
	dw	?int40		; 40col
	dw	?pt$i$1101	; prt1
	dw	?pt$i$1101	; prt2
	dw	?int65		; 6551
   if	not use$6551
	dw	init$RS232	; software RS232
   endif
	dw	rret		;
max$devices	equ	(($-number$drivers)/2)-1

;
;
;
?ci:				; character input
	call	vector$io	; jump with table adr on stack
	dw	key$board$in	; keys
	dw	rret		; 80col
	dw	rret		; 40col
	dw	rret		; ptr1
	dw	rret		; prt2
	dw	?in65		; 6551
   if	not use$6551
	dw	in$RS232	; software RS232
   endif
	dw	null$input

;
;
;
?cist:				; character input status
	call	vector$io	; jump with table adr on stack
	dw	key$board$stat	; keys
	dw	rret		; 80col
	dw	rret		; 40col
	dw	rret		; prt1
	dw	rret		; prt2
	dw	?ins65		; 6551
   if	not use$6551
	dw	in$st$RS232	; software RS232
   endif
	dw	rret

;
;
;
?co:				; character output
	call	vector$io	; jump with table adr on stack
	dw	rret		; keys
	dw	?out80		; 80col
	dw	?out40		; 40col
	dw	?pt$o$1		; prt1
	dw	?pt$o$2		; prt2
	dw	?out65		; 6551
   if	not use$6551
	dw	out$RS232	; software RS232
   endif
	dw	rret

;
;
;
?cost:				; character output status
	call	vector$io	; jump with table adr on stack
	dw	ret$true	; keys
	dw	ret$true	; 80col
	dw	ret$true	; 40col
	dw	ret$true	; prt1	?pt$s$1101
	dw	ret$true	; prt2
	dw	?outs65		; 6551
   if	not use$6551
	dw	out$st$RS232	; software RS232
   endif
	dw	ret$true

	page
;
;	This entry does not care about values of DE
;
vector$io:
	mvi	a,max$devices	; check for device # to high
	mov	e,b		; get devive # in E
;
;
;	INPUT:
;		Vector # in E, Max device in A
;		passes value in DE$TEMP in DE
;		HL has routine's address in it on entering routine
;
;	OUTPUT:
;		ALL registers of returning routine are passed
;
vector:
	pop	h		; get address vector list
	mvi	d,0		; zero out the MSB
	cmp	e		; is it too high?
	jrnc	exist		; no, go get the handler address

	mov	e,a		; yes, set to max$dev$handler(last one) 
exist:
	dad	d		; 
	dad	d		; point into table

 	mov	a,m
	inx	h
	mov	h,m
	mov	l,a		; get routine adr in HL

    if	banked
	shld	hl$temp		; save exec adr
	lxi	h,0
	dad	sp
	lxi	sp,bios$stack
	push	h		; save old stack

	lhld	de$temp
	xchg
	lhld	hl$temp		; recover exec adr

	lda	force$map	; get current bank
	push	psw		; save on stack
	sta	bank$0		; set bank 0 as current

	call	ipchl

	sta	a$temp		; save value to return
	pop	psw
	sta	force$map	; set old bank back
	lda	a$temp		; recover value to return

	shld	hl$temp
	pop	h		; recover old stack
	sphl			; set new stack
	lhld	hl$temp
	ret

ipchl:
	pchl			; jmp to handler

	ds	30h
bios$stack:

    else
	lda	a$temp
	xchg
	lhld	de$temp
	xchg
	pchl
    endif

	page
;==========================================================
;		CHARACTER INPUT ROUTINES
;==========================================================

	DSEG
;
;
;
key$board$in:
	call	key$board$stat	; test if key is available
	jrz	key$board$in	

	lda	key$buf
	push	psw		; save on stack
	xra	a		; clear key 
	sta	key$buf
;
;**	the tracking of the display should be able to be turned off
;**	this could be done with one of the keyboard's Fx codes
;
	lda	stat$enable
	bit	6,a
	jrnz	no$update
	lda	char$col$40
	mov	b,a
	lda	@off40
	cmp	b
	jrnc	do$update
	adi	39-1
	cmp	b
	jrnc	no$update
do$update:
	mvi	a,80h	
	sta	old$offset	; store 80h to demand update
no$update:
	pop	psw		; recover current key
rret:
	ret

;
;
;
null$input:		; return a ctl-Z for no device
	mvi	a,1Ah
	ret


	page

;==========================================================
;	CHARACTER DEVICE INPUT STATUS
;==========================================================

	DSEG
;
;
;
key$board$stat:
	lda	key$buf
	ora	a
	jrnz	ret$true

	call	?get$key
	ora	a		; =0 if none
	rz			; return character not advailable

	sta	key$buf		; was one, save in key buffer

ret$true:
	ori	0ffh		; and return true
	ret

	page

	cseg
@ctbl
	db	'KEYS  '	; device 0, internal keyboard
	db	mb$input
	db	baud$none

	db	'80COL '	; device 1, 80 column display
	db	mb$output
	db	baud$none

	db	'40COL '	; device 2, 40 column display
	db	mb$output
	db	baud$none

	db	'PRT1  '	; device 3, serial bus printer (device 4)
	db	mb$output
	db	baud$none

	db	'PRT2  '	; device 4, serial bus printer (device 5)
	db	mb$output
	db	baud$none

	db	'6551  '	; device 5, EXT CRT
	db	mb$in$out+mb$serial+mb$softbaud+mb$xonxoff
?intbd:
	db	baud$1200
   if	not use$6551
	db	'RS232 '	; device 6, software RS232 device
	db	mb$in$out+mb$serial+mb$xonxoff+mb$softbaud
RS232$baud:
	db	baud$300
   endif
	db	0		; mark end of table

	page
;
;	TIME.ASM
;
	cseg
;
;	HL and DE must be presevered
;
?time:
	inr	c
	lxi	b,cia$hours
	jrz	set$time
;
;	update SCB time  (READ THE TIME)
;
	inp	a			; read HR (sets sign flag)
	jp	is$am			; jmp if AM (positive)
	ani	7fh
	adi	12h			; noon=24(PM), midnight=12(AM)
	daa
	cpi	24h			; check for noon (12+12 PM)
	jrnz	set$hr
	mvi	a,12h
	jr	set$hr

is$am:
	cpi	12h			; check for midnight (AM)
	jrnz	set$hr
	xra	a			; becomes 00:00
set$hr:
	sta	@hour
	mov	b,a
	lda	old$hr
	mov	c,a
	mov	a,b
	sta	old$hr
        cmp     c                       ; if @hour<old$hr
	jrnc	same$day
 
	push	h
	lhld	@date
	inx	h
	shld	@date
	pop	h

same$day:
	lxi	b,cia$hours-1
	inp	a			; read MIN
	sta	@min

	dcr	c
	inp	a			; read SEC
	sta	@sec

	dcr	c
	inp	a			; read 1/10 of SEC (a must to free
	ret				; the holding register)

old$hr:
	ds	1

	page
;
;
;
set$time
	lda	@hour
	sta	old$hr
	cpi	12h			; test for noon
	jrz	set$as$is
	ana	a			; test for 00:xx
	jrnz	not$zero$hundred
	mvi	a,80h+12h			; set to midnight
	jr	set$as$is

not$zero$hundred:
 	cpi	11h+1			; test for 1 to 11 AM
	jrc	set$as$is
	sui	12h
	daa				; decimal adjust
set$msb:
	ori	80h			; set PM

set$as$is:
	outp	a
	dcr	c
	lda	@min
	outp	a
	dcr	c
	lda	@sec
	outp	a
	dcr	c
	xra	a
	outp	a
	ret

	page
;
; CXMOVE.ASM
;
	public ?move,?xmove,?bank

;
;	Move a block of data from DE to HL
;	count is in BC (within current bank)
;
;
	cseg			; place code in common
?move:
	xchg			;*
	lda	source$bnk	; =FFh if normal block move 
	inr	a		; 
	jrnz	inter$bank$move

	LDIR			;* do block move	
	xchg			;*
	ret


;
;
;
?xmove:				; can be in bank 0	
	mov	a,c
	sta	source$bnk
	mov	a,b
	sta	dest$bnk
	ret			;*

	page
;
;
;
inter$bank$move:
	shld	@buffer		; save HL TEMP
	lxi	h,0
	dad	sp
	lxi	sp,bios$stack
	push	h		; save old stack  ;**1
	lhld	@buffer

inter$bank$move$1:
	mov	a,b		; get msb of count
	ora	a
	jrz	count$less$than$256
	push	b		; save the count  ;**2
	push	d		; save the dest   ;**3
	lxi	d,@buffer	; make buffer the dest
	lxi	b,256		; move 256 bytes
	lda	source$bnk
	call	?bank
	ldir			; move source to buffer

	pop	d		; recover dest    ;**2
	push	h		; save updated source ;**3
	lxi	h,@buffer	; make the buffer the source
	lxi	b,256		; move 256 bytes
	lda	dest$bnk
	call	?bank
	ldir			; move buffer to dest
 
	pop	h		; recover updated source ;**2
	pop	b		; recover count          ;**1
	dcr	b		; subtract 256 from count
	jr	inter$bank$move$1

	page
;
;
;
count$less$than$256:
	ora	c		; BC=0  [A (0) or'ed with C]
 	jrz	exit$move

	push	d		; save count for 2nd half  ;**2
	push	b		; save dest adr            ;**3
	lxi	d,@buffer
	lda	source$bnk
	call	?bank
	ldir			; move source to buffer

	pop	b		; recover count		  ;**2
	pop	d		; recover dest		  ;**1
	push	h		; save updated dest	  ;**2
	lxi	h,@buffer
	lda	dest$bnk
	call	?bank
	ldir			; move buffer to dest
	pop	h	 				   ;**1
;
;
;
exit$move:
	xchg
	mvi	a,-1
	sta	source$bnk	; set MOVE back to normal
	lda	@cbnk

	shld	@buffer
	pop	h		; recover old stack	;**0
	sphl
	lhld	@buffer

; call	?bank		; set current bank
; ret

	page
;
;	switch bank to bank number in A
;
	cseg			; (must be in common)
?bank:				
   if	banked
	ora	a		; bank 0 ?
	jrnz	not$bank$0	; go check for bank 1

	sta	bank$0		; set bank 0
	ret

;
;
not$bank$0:
	dcr	a		; bank 1 ?
	rnz			; if not a valid bank just return
	sta	bank$1		; set bank 1
   endif
	ret

	end

