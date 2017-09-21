	title	'C128 Kerberos disk support  11 Sep 2017'

	maclib	z80

	maclib	cxequ

	extrn	@dtbl		; DMA ram bank
	extrn	?dkmov$hl

	public	krb$is$present
	public	krb$sram$set$bank
	public	krb$buffer$to$dma

	public	kbdsk

dcxf	MACRO	?H,?L
	; 16 decrement that sets carry flag correctly
	LOCAL	done
	dcr	l
	jrnc	done
	dcr	h
done:
	ENDM


	page
;
	CSEG		; place code in common memory

sram$sector$bank	EQU	200h-16

;
; Extended Disk Parameter Headers (XPDHs)
;
	dw	kbdsk$write
	dw	kbdsk$read
	dw	kbdsk$login
	dw	kbdsk$init
	db	0		; relative drive zero
	db	0		; format type byte
kbdsk:	
	dw	0			; TRANSLATE TABLE ADDRESS
	db	0,0,0,0,0,0,0,0,0	; BDOS SCRATCH AREA
     	db	0			; MEDIA FLAG
	dw	dpb$kb			; DISK PARAMETER BLOCK
	dw	00000h			; CHECKSUM VECTOR ALLOCATED BY
	dw	0FFFEh			; ALLOC VECTOR ALLOCATED BY GENCPM
	dw	0FFFEh			; DIRBCB
	dw	0FFFEh			; DTABCB
	dw	0FFFEh			; HASH ALLOC'D
	db	0			; HASH BANK


	;
	; DPB FOR Kerberos flash disk
	;

	; AU - 2048 bytes
	; SECTOR - 256 bytes
	; TRACK - 8 KiB bank
	; sectors/track - 32
	; 48 tracks
dpb$kb:
	DW	0064		; SPT - 128 BYTE RECORDS PER TRACK
	DB	04,0Fh		; BSH, BLM - BLOCK SHIFT AND MASK
	DB	01		; EXM - EXTENT MASK
	DW	00BFh		; DSM - MAXIMUM AU NUMBER
	DW	007Fh		; DRM - MAXIMUM DIRECTORY ENTRY NUMBER
	DB	0C0h,00h	; AL0, AL1 - ALLOC VECTOR FOR DIRECTORY
	DW	8000h		; CKS - CHECKSUM SIZE
	DW	0		; OFF - OFFSET FOR SYSTEM TRACKS
	DB	1,1		; PHYSICAL SECTOR SIZE SHIFT


kbdsk$init:
	call	krb$is$present
	ora	a
	rnz				; found Kerberos
	
;
;	device is missing, remove vector
;
	lxi	h,0			; remove vector to flash disk
	shld	@dtbl+('K'-'A')*2	; .. (drive K:)
kbdsk$login:
	ret

;
; disk READ and WRITE entry points.
; These entries are called with the following arguments:
;	relative drive number in @rdrv (8 bits)
;	absolute drive number in @adrv (8 bits)
;	disk transfer address in @dma (16 bits)
;	disk transfer bank	in @dbnk (8 bits)
;	disk track address	in @trk (16 bits)
;	disk sector address	in @sect (16 bits)
;	pointer to XDPH in <DE>
;
;   return with an error code in <A>
;	A=0	no errors
;	A=1	non-recoverable error
;	A=2	disk write protected
;	A=FF	media change detected
;

kbdsk$write:
	mvi	a,1			; set error
	ret

; hl - CPU address sector
copy$sram$to$flash:
	push	b
	push	d
	push	hl

	call	erase$sector

	lxi	d,sram$sector$bank
	call	krb$sram$set$bank

	lxi	b,krb$sram
	lxi	d,01000h-1

flash$next$byte:
	inp	a
	cpi	0FFh
	jrz	do$next$byte

	push	d
	push	psw

	lded	krb$flash$addr

	call	prepare$write

	mvi	a,0A0h
	sta	08AAAh		; Cycle 3 - AAAh <- 80h

	call	krb$flash$set$bank

	pop	psw
	pop	d

	mov	m,a		; Cycle 4 - write data

	; wait 10 us - 20 cycles at 2 MHz
	; The next commands take longer

do$next$byte:
	dcxf	de			; 6
	jrc	flash$done		; 7
	inx	hl			; 6
	inr	c			; 4
	cz	krb$sram$set$next$bank	; 10
	jr	flash$next$byte		; 12
	
flash$done:
	pop	hl
	pop	d
	pop	b


; hl - CPU address sector
erase$sector:
	push	d
	lded	krb$flash$addr

	call	prepare$write

	mvi	a,080h
	sta	08AAAh		; Cycle 3 - AAAh <- 80h
	mvi	a,0AAh
	sta	08AAAh		; Cycle 4 - AAAh <- AAh
	cma
	sta	08555h		; Cycle 5 - 555h <- 55h

	call	krb$flash$set$bank

	mvi	m,050h		; Cycle 6 - Sector base <- 50h

	; wait 25 ms - 50000 cycles at 2 MHz
	lxi	d,2800-1
erase$wait:
	dcxf	d		; 6
	jrnc	erase$wait	; 12

	pop	d
	ret

prepare$write:
	call	krb$flash$set$bank$0

	mvi	a,0AAh
	sta	08AAAh		; Cycle 1 - AAAh <- AAh
	cma
	sta	08555h		; Cycle 2 - 555h <- 55h
	ret



; hl - CPU address sector
copy$sector$to$sram:
	lxi	d,sram$sector$bank
copy$next$bank:
	call	krb$sram$set$bank
	lxi	b,krb$sram
copy$next$byte:
	mov	a,m
	outp	a
	inx	h
	inr	c
	jrnz	copy$next$byte
	inr	e
	jrnz	copy$next$bank
	ret


kbdsk$read:
	lded	@trk
	call	krb$disk$set$bank

	di

	lxi	h,force$map
	mov	a,m
	push	psw
	mvi	m,00111011b	; 00 - BANK 0, 11 - RAM, 10 - EXT ROM, 1 - RAM, 1 - RAM/ROM

	lda	@sect
	adi	080h
	mov	h,a
	xra	a
	mov	l,a
	lxi	d,@buffer
	lxi	b,0100h
	lddr

	pop	psw
	sta	force$map

	ei

krb$buffer$to$dma:
	lhld	@dma
	call	?dkmov$hl	; A=0 transfers data from buffer to local$DMA
	xra	a
	ret

krb$is$present:
	lda	krb$status
	ora	a
	rp

;	Detect kerberos SRAM

	xra	a
	lxi	b,krb$cart$ctl
	outp	a
	inr	c
	outp	a

	call	krb$sram$set$bank$0

	lxi	b,krb$sram
	xra	a
	outp	a

	lxi	d,01FFh			; select bank 1FFh
	call	krb$sram$set$bank

	lxi	b,krb$sram
	dcr	a
	outp	a
	inp	e
	cmp	e
	jrnz	krb$missing

	call	krb$sram$set$bank$0

	lxi	b,krb$sram
	inp	a
	jrnz	krb$missing	; found Kerberos

	inr	a
	jr	krb$detect$done

krb$missing:
	xra	a
krb$detect$done:
	sta	krb$status
	ret


krb$flash$set$bank$0:
	push	d
	lxi	d,0
	jr	krb$flash$do$bank

;	de - bank 0-47
krb$disk$set$bank:
	push	d
	mov	a,e
	adi	8
	mov	e,a
krb$flash$do$bank:
	call	krb$flash$set$bank
	pop	d
	ret

;	de - bank 0-1FF
krb$flash$set$bank:
	sded	krb$flash$addr
	jr	krb$set$banks

krb$sram$set$next$bank:
	push	d
	lded	krb$sram$addr
	inx	d
	jr	krb$sram$do$bank

krb$sram$set$bank$0:
	push	d
	lxi	d,0			; select bank 0
krb$sram$do$bank:
	call	krb$sram$set$bank
	pop	d
	ret

;	de - bank 0-1FF
krb$sram$set$bank:
	sded	krb$sram$addr

krb$set$banks:
	push	b
	push	d
	push	h

	lxi	b,krb$flash$bank
	lxi	h,krb$flash$addr

	mov	a,m	; flash lo byte
	outp	a

	inx	h
	mov	a,m	; flash hi byte
	add	a

	inx	h
	mov	d,m	; sram lo byte
	inx	b
	outp	d

	inx	h
	ora	m
	inx	b
	outp	a

	pop	h
	pop	d
	pop	b
	ret

krb$flash$addr:
	dw	0h
krb$sram$addr:
	dw	0h

krb$status:
	db	0FFh
