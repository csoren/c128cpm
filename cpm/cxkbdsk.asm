	title	'C128 Kerberos disk support  11 Sep 2017'

	maclib	z80

	maclib	cxequ

	extrn	@dtbl		; DMA ram bank

	public	kerberos$is$present
	public	kerberos$sram$set$bank
	public	kerberos$buffer$to$dma

	public	kbdsk

	page
;
	CSEG		; place code in common memory

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
	call	kerberos$is$present
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

kbdsk$read:
	lhld	@trk
	call	kerberos$flash$set$bank

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

kerberos$buffer$to$dma:
	lhld	@dma
	call	?dkmov$hl	; A=0 transfers data from buffer to local$DMA
	xra	a
	ret

kerberos$is$present:
	lda	kerberos$status
	ora	a
	rp

;	Detect kerberos SRAM

	xra	a
	lxi	b,kerb$cart$ctl
	outp	a
	inr	c
	outp	a

	call	kerberos$sram$set$bank$0

	lxi	b,kerb$sram
	xra	a
	outp	a

	lxi	hl,1FFh			; select bank 1FFh
	call	kerberos$sram$set$bank

	lxi	b,kerb$sram
	dcr	a
	outp	a
	inp	e
	cmp	e
	jrnz	kerberos$missing

	call	kerberos$sram$set$bank$0

	lxi	b,kerb$sram
	inp	a
	jrnz	kerberos$missing	; found Kerberos

	inr	a
	jr	kerberos$detect$done

kerberos$missing:
	xra	a
kerberos$detect$done:
	sta	kerberos$status
	ret


;	hl - bank 0-1FF
kerberos$flash$set$bank:
	lxi	b,kerb$flash$bank
	outp	l
	mvi	c,LOW kerb$bank$hi
	slar	h
	outp	h
	ret

kerberos$sram$set$bank$0:
	lxi	hl,0			; select bank 0

;	hl - bank 0-1FF
kerberos$sram$set$bank:
	lxi	b,kerb$sram$bank
	outp	l
	inr	c
	outp	h
	ret

kerberos$status:
	db	0FFh
