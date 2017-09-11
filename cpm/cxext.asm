;
;
	title	'C128 external Disk drive support  28 Apr 86'

;
;	This program contains the stubs for bringing up the C128 CP/M
;	for the first time.
;
;	The method used to stub the system I/O is to send the
;	operation request to the serial port as a command and
;	recieve responce from the serial channel.
;
;	The commands supported are:
;
;	CMD:	'I'	; input keyboard char
;	RSP:	xx	; returns keybord char or 00 if none
;
;	CMD	'O'xx	; send char xx to display 
;	RSP:	xx	; echo character
;
;	CMD:	Rttss	; read sector of data  adr by track (tt) sector (ss) 
;	RSP:	xx..yy	; returns 128 bytes of data plus a check sum
;
;	CMD:	Wttssxx..yy	; write sector of data, sent with a check sum 
;				; to (xx..yy) adr by track (tt) sector (ss)
;	RSP:	xx		; xx=00 if no error
;
	page

	maclib	cpm3

	maclib	z80

	maclib	cxequ



	public	?int65,?in65,?ins65,?out65,?outs65
	extrn	?intbd

; Utility routines in standard BIOS
	extrn	?pmsg		; print message @<HL> up to 00
				; saves <BC> & <DE>
	extrn	?pdec		; print binary number in <A> from 0 to 99.
	extrn	?pderr		; print BIOS disk error header
	extrn	?conin,?cono	; con in and out
	extrn	?const		; get console status

;
;	drive table
;
	public	@dtbl
	extrn	cmdsk0,cmdsk1,cmdsk2,cmdsk3,cmdsk4,RMdsk,kbdsk

	page
;
;	DRVTBL.ASM		
;
	CSEG		; place code in common memory

@dtbl:
	dw	cmdsk0			;* drive A 1541/1571 
	dw	cmdsk1			;* drive B 1541/1571
	dw	cmdsk2			;* drive C 1541/1571
	dw	cmdsk3			;* drive D 1541/1571
	dw	cmdsk4			;* drive E shares drive A
	dw	0			;* drive F 
	dw	0			;* drive G
    if	EXTSYS
	dw 	@fdsd0			;* drive H (external RS232)
    else
	dw	0			;* drive H
    endif
	dw	0			;* drive I
	dw	0			;* drive J
	dw	kbdsk			;* drive K  Kerberos flash disk
	dw	0			;* drive L
	dw	RMdsk			;* drive M  Memory Disk (RAM disk)
	dw	0			;* drive N
	dw	0			;* drive O
	dw	0			;* drive P


    if	EXTSYS
	CSEG
;
; Extended Disk Parameter Headers (XPDHs)
;
	dw	fd$write
	dw	fd$read
	dw	fd$login
	dw	fd$init
	db	0		; relative drive zero
	db	0		; format type byte
@fdsd0:	
	dph	sk128sssd,dpb$8$sssd


;
; DPB FOR 8 IBM 3740 format		( 243K )
;
dpb$8$sssd:	dpb	128,26,77,1024,64,2

sk128sssd:
	skew	26,6,1




	page
;
;	send an illegial command, should get a period back, meaning
;	that the the command was bad. at this point extrnal system
;	is ready to receive a valid command.
;
	CSEG
resync:
	mvi	c,0dh
	call	send$c
	call	get
	cpi	'.'
	jrnz	resync

	mvi	c,'O'
	call	send$c
	mvi	c,07			; beep the bell
	call	send$c
	call	get			; should be a bell code
	cpi	07
	rz

	call	get
	jr	resync



;
;	CXDISK.ASM
;

;
;
;
	dseg
fd$read:
	mvi	a,10
	sta	error$count		; set retrys to 10
retry$read:
	lxi	h,retry$read
	push	h			; save retry address on the stack
	mvi	a,'R'
	call	set$up$dsk		; send command, track and sector
read$loop:
	call	get

   if	banked
	call	put$byte$de$bank	; save byte disk bank
	mov	a,c
   else
	mov	m,a
   endif

	inx	h
	call	do$sum
	jrnz	read$loop

	call	get
	lda	check$sum
	cmp	c
	jrnz	dsk$error

	pop	h			; remove retry address
	xra	a			; A=0 (no errors)
;
;
fd$init:
fd$login:
	ret


;
;
;
   if	banked

	cseg
put$byte$de$bank:
	lda	force$map		; read current MMU configuration
	stax	d			; force to preconfig reg adr in DE
	mov	m,c			; save C in proper bank
	sta	force$map		; force the old bank back
	ret
   endif

	page
;
;
;
	dseg
dsk$error:
	call	resync
	lda	error$count
	dcr	a
	sta	error$count
	rnz				; return to retry address

	inr	a			; A=1 if hard error
	pop	h			; remove retry address on error
	ret

error$count:	db	0

	page
;
;
;
	dseg
fd$write:
	mvi	a,10
	sta	error$count		; set retrys to 10

retry$write:
	lxi	h,retry$write
	push	h
	mvi	a,'W'
	call	set$up$dsk		; send command, track and sector

write$loop:
   if	banked
	call	get$byte$de$bank
   else
	mov	c,m
   endif
	call	send$c			; leaves sent char in A
	inx	h
	call	do$sum
	jrnz	write$loop
	lda	check$sum
	call	send$a
	call	get
	ora	a			; A=0 if no errors
	jrnz	dsk$error

	pop	h			; remove error address
	ret				; A=0 (no errors)


   if	banked

	cseg
get$byte$de$bank:
	lda	force$map		; read current MMU configuration
	stax	d			; set current disk bank (in DE)
	mov	c,m
	sta	force$map		; write current MMU conf back
	ret
   endif

	page
;
;	compute check sum and adjust byte count
;	
	dseg
do$sum:
	mov	b,a
	lda	check$sum		; get the current sum
	add	b			; add in new byte
	sta	check$sum		; save new sum
	lda	count			; get byte count
	dcr	a			; one less to get
	sta	count			; save for later
	ret				; zero flag set if DONE

check$sum:	db	0
count:		db	0


;
;	send the command, track and sector to the external system
;	set count to 128 bytes, clear the checksum and set HL to
;	the DMA address
;
set$up$dsk:
	call	send$a		; send the comand
	lda	@trk
	call	send$a		; send the track
	lda	@sect
	call	send$a		; send the sector
	xra	a
	sta	check$sum
	mvi	a,80h
	sta	count		; transfer 128 bytes
	lhld	@dma		; HL = current DMA address

   if	banked
	lxi	d,bank$0	; start by pointing to bank 0
	lda	@dbnk		; get the current disk I/O bank
	ora	a		; is it set to bank 0
	rz			; yes, return

	inx	d		; no, point to bank 1
   endif
	ret

	page

;==========================================================
;		CHARACTOR INITILIZATION ROUTINES
;==========================================================
;
;
;
	dseg

;
;	set external system com rate to 19.2 K baud
;
?int65:
init$ext:
	lhld	usart$adr
	mov	b,h
	mov	c,l
	inx	b
	inx	b			; point to command reg

	mvi	a,cmd$init
	outp	a
	inx	b			; point to control reg
	mvi	a,cntr$init$19200	; baud rate equ 19200
	outp	a

	dcx	b			; (02)
	dcx	b			; (01)
	inp	a			; read status
	dcx	b			; (00)
	inp	a			; read hung data
	ret

	page
;==========================================================
;		CHARACTOR INPUT ROUTINES
;==========================================================

;
;
;
	dseg
?in65:				; character input
	call	?ins65		; check for character adv.
	jrz	?in65		; loop if NOT
	lda	key		; get the key code
	push	psw		; save on stack
	xra	a		; clear key
	sta	key
	pop	psw		; recover current key
	ret

	page

;==========================================================
;	CHARACTER DEVICE INPUT STATUS
;==========================================================

;
;
;
	dseg
?ins65:				; character input status
	lda	key		; is there already a key
	ora	a
	jrnz	ret$true	; yes, return true
	mvi	a,'I'		; no, test if any typed
	call	send$a
	call	get		; get key
	ora	a		; =0 if none
	rz			; return character not advaliable

	sta	key		; was one, save in key 
?outs65:
ret$true:
	ori	0ffh
	ret

key:	db	0

	page
;==========================================================
;	CHARACTER DEVICE OUTPUT
;==========================================================

; the charactor to be output is in the C register

;
;
;
	dseg
?out65:				; character output
	mov	a,c
	push	psw
	mvi	a,'O'
	call	send$a
	pop	psw
	call	send$a
;	jmp	get

; fall thru to GET

;==========================================================
;		EXTERNAL DEVICE LOW LEVEL DRIVERS
;==========================================================

;
;
;
	dseg
get:
	call	in$stat
	jrz	get
	dcx	b		; point to data reg (RxD)
	inp	a
	mov	c,a
	ret
;
;
;
send$c:
	mov	a,c
send$a:
	push	psw		; save the character to be output
send$loop:
	call	out$stat
	jrz	send$loop
	pop	psw
	dcx	b		: point to data register (TxD)
	outp	a
	ret
;
;
;
in$stat:
	lhld	usart$adr
	mov	b,h
	mov	c,l
	inx	b		; point to status register
	inp	a
	ani	rxrdy
	ret
;
;
;
out$stat:
	lhld	usart$adr
	mov	b,h
	mov	c,l
	inx	b		; point to status register
	inp	a
	ani	txrdy
	ret
    else
;==========================================================
;		CHARACTOR INITILIZATION ROUTINES
;==========================================================
;
;
;
	dseg
;
;	set com rate to value in ?int$bd
;	(may need to change rate if not supported)
?int65:
init$ext:
   if	use$6551
;
;	must gate 6551 to user port
;	this is done by init'ing the out data to an input
;	and then setting DTR
;
 	lxi	b,CIA2+data$dir$a
	inp	a
	ori	100b			; make TxD bit (2) an input
	outp	a
  endif

	lhld	usart$adr
	mov	b,h
	mov	c,l
	inx	b			; point to status reg
	outp	a			; software reset (wr to stat reg)

	inx	b			; point to Command register
	mvi	a,cmd$init		; set DTR active
	outp	a

	inx	b			; point to Control register
	lda	?int$bd			; get 6551 baud rate
	ori	10h			; use baud rate generator
	outp	a			; 1 stop (7=0), 8 bits (65=0)
	ret

	page
;==========================================================
;		CHARACTOR INPUT ROUTINES
;==========================================================

;
;
;
	dseg
?in65:				; character input
	call	?ins65
	jrz	?in65
	dcx	b		; point to data reg
	inp	a
	ret

;==========================================================
;	CHARACTER DEVICE INPUT STATUS
;==========================================================
;
;
;
	dseg
?ins65:			; character input status
	lhld	usart$adr
	mov	b,h
	mov	c,l
	inx	b		; point to status register
	inp	a
	ani	rxrdy
	rz
	ori	-1
	ret


;==========================================================
;	CHARACTER DEVICE OUTPUT
;==========================================================

; the charactor to be output is in the C register

;
;
;
	dseg
?out65:				; character output
	mov	a,c
	push	psw
again65:
	call	?outs65
	jrz	again65
	pop	psw
	dcx	b		; point to data register
	outp	a
	ret

;==========================================================
;	CHARACTER DEVICE OUTPUT STATUS
;==========================================================
;
;
;
	dseg
?outs65:			; character input status
	lhld	usart$adr
	mov	b,h
	mov	c,l
	inx	b		; point to status register
	inp	a
	ani	txrdy
	rz
	ori	-1
	ret


    endif


	end

