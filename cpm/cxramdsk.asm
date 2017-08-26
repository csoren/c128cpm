;
;
	title	'C128 Ram Disk support  14 Oct 85'


;	maclib	cpm3

	maclib	z80

	maclib	cxequ



; Utility routines in standard BIOS
        extrn   ?pmsg           ; print message @<HL> up to 00
                                ; saves <BC> & <DE>
        extrn   ?pdec           ; print binary number in <A> from 0 to 99.
	extrn	?pderr		; print BIOS disk error header
	extrn	?conin,?cono	; con in and out
	extrn	?const		; get console status
	extrn	@dtbl		; DMA ram bank

	public	RMdsk

	extrn	?fun65
	extrn	?dkmov

	page
;
	CSEG		; place code in common memory

;
; Extended Disk Parameter Headers (XPDHs)
;
	dw	RM$write
	dw	RM$read
	dw	RM$login
	dw	RM$init
	db	0		; relative drive zero
	db	0		; format type byte
RMdsk:		;	dph	0,dpb$RM$512
	dw	0			; TRANSLATE TABLE ADDRESS
	db	0,0,0,0,0,0,0,0,0	; BDOS SCRATCH AREA
     	db	0			; MEDIA FLAG
DPB$ptr:
	dw	dpb$RM$512		; DISK PARAMETER BLOCK
	dw	00000h			; CHECKSUM VECTOR ALLOCATED BY
	dw	0FFFEh			; ALLOC VECTOR ALLOCATED BY GENCPM
	dw	0FFFEh			; DIRBCB
	dw	0FFFEh			; DTABCB
	dw	0FFFEh			; HASH ALLOC'D
	db	0			; HASH BANK


                ;
                ; DPB FOR RAM disk
                ;

dpb$RM$128:	;	dpb	256,1,512,1024,64,0
	DW	0002		; 128 BYTE RECORDS PER TRACK
	DB	03,07		; BLOCK SHIFT AND MASK
	DB	00		; EXTENT MASK
	DW	007Fh		; MAXIMUM BLOCK NUMBER
	DW	003Fh		; MAXIMUM DIRECTORY ENTRY NUMBER
	DB	0C0h,00h	; ALLOC VECTOR FOR DIRECTORY
	DW	8000h		; CHECKSUM SIZE
	DW	0		; OFFSET FOR SYSTEM TRACKS
	DB	1,1		; PHYSICAL SECTOR SIZE SHIFT

dpb$RM$512:	;	dpb	256,1,2048,2048,128,0
	DW	0002		; 128 BYTE RECORDS PER TRACK
	DB	04,0Fh		; BLOCK SHIFT AND MASK
	DB	01		; EXTENT MASK
	DW	00FFh		; MAXIMUM BLOCK NUMBER
	DW	007Fh		; MAXIMUM DIRECTORY ENTRY NUMBER
	DB	0C0h,00h	; ALLOC VECTOR FOR DIRECTORY
	DW	8000h		; CHECKSUM SIZE
	DW	0		; OFFSET FOR SYSTEM TRACKS
	DB	1,1		; PHYSICAL SECTOR SIZE SHIFT

	page
;
;
;
	dseg
RM$write:
	mvi	d,VIC$RM$wr
	lda	@dbnk		; get disk bank
	ana	a
	lhld	@dma
	jrz	RM$do$rd$wr
        call    ?dkmov+3        ; A<>0 transfers data from local$DMA to buffer
	mvi	d,VIC$RM$wr
	jr	RM$do$rd$wr$buf
;
;
;
RM$read:
	mvi	d,VIC$RM$rd
	lda	@dbnk		; get disk bank
	ana	a		; is it bank zero
	lhld	@dma
	jrz	RM$do$rd$wr	; yes, go read it

	call	RM$do$rd$wr$buf	; no,  transfer through buffer
	lhld	@dma
	call	?dkmov+3	; A=0 transfers data from buffer to local$DMA
	xra	a
	ret
;
;
;
RM$do$rd$wr$buf:
	lxi	h,@buffer
RM$do$rd$wr:
	lxi	b,RM$128$low
	outp	l
	inr	c		; RM$128$mid
	outp	h
	inr	c		; RM$ext$low
	xra	a
	outp	a
	lhld	@trk
	inr	c		; RM$ext$mid
	outp	l
	inr	c		; RM$ext$hi
	outp	h
	lxi	h,256
	inr	c		; RM$count$low
	outp	l
	inr	c		; RM$count$hi
	outp	h
	mov	a,d		; get rd/wr command
	call	?fun65
	xra	a		; set no errors
	ret

	page
;
;
;
	dseg
RM$init:
	lxi	b,RM$control
	xra	a
	outp	a			; increment both addresses
	dcr	c			; point to interrupt control register
	outp	a			; disable interrupts

	lxi	h,0			; point to track 0
	shld	@trk
	xra	a
	sta	@dbnk			; set DMA bank to zero
	lxi	h,@buffer		; 
	shld	@dma

test$device$present:
	mov	m,l			; place a pattern in the directory
	inr	l			; ..buffer area
	jrnz	test$device$present	; 

	call	RM$read			; read track 0 to DMA buffer
	lxi	h,@buffer		; ..(buffer not changed if
	lxi	d,dir$label		; ..device is not present)
	lxi	b,12			; test if KEY has been installed
test$next$key:
	ldax	d
	inx	d
	cci
	jrnz	no$match		; KEY missing, test device present
	jpe	test$next$key
	jr	set$size		; KEY is in RAM DISK, go set size

	page
;
;	test if device is present, remove vector if not
;
no$match:
	mvi	l,0			; start back at the buffer beginning
test$for$ram$dsk:
	mov	a,m
	cmp	l			; buffer changed?
	jrnz	device$is$present	; yes, then device is present
	inr	l			; no, buffer end?
	jrnz	test$for$ram$dsk	; no, test rest of buffer
					; yes, L=0
;
;	device is missing, remove vector
;
	mov	h,l			; remove vector to RAM disk
	shld	@dtbl+('M'-'A')*2	; .. (drive M:)
	ret
;
;	initialize directory buffer
;
device$is$present:
	call	init$buffer		; fill buffer with E5`s
	lxi	h,dir$label
	lxi	d,@buffer
	lxi	b,32
	ldir				; install directory label in 1st record
	lxi	h,0
	shld	@trk			; set track=0

clear$dir:
	call	RM$write		; erase director sectors
	call	init$buffer		; fill buffer with E5`s
	lda	@trk
	inr	a
	sta	@trk
	cpi	16			; 16 for 512K Ram disk
	jrnz	clear$dir

set$size:
	lxi	h,dpb$RM$128
	lxi	b,RM$status
	inp	a
	ani	10h			; mask of size bit (0=128K)
	jrz	set$128K
	lxi	h,dpb$RM$512
set$128K:
	shld	dpb$ptr	
RM$login:
	ret

	page
;
;
;
init$buffer:
	lxi	h,@buffer
	mvi	m,0E5h
	lxi	d,@buffer+1
	lxi	b,256-1
	ldir
	ret

;
;
;
dir$label:	;123456789012  3 4 5 6
	db	' ERTWINE VON',1,0,0,0
	dw	0,0,0,0
	dw	date$hex,0
	dw	date$hex,0

