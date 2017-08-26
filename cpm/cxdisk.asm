


;
;	*****************************************
;	*					*
;	*	Commodore Disk	Controller	*
;	*	Module for CP/M 3.0 BIOS	*
;	*					*
;	*****************************************
;
;
;
	title 'CXDISK   Commodore C-128 Disk Controller    15 Apr 86'




; CP/M 3 Disk definition macros

	maclib	cpm3

	maclib	z80

; C-128 system equates

	maclib	cxequ

	page

; Disk drive dispatching tables for linked BIOS

	public	cmdsk0,cmdsk1,cmdsk2,cmdsk3,cmdsk4

; System Control Block variables
	extrn	@ermde		; BDOS error mode

; Utility routines in standard BIOS
	extrn	?wboot		; warm boot vector
        extrn   ?pmsg           ; print message @<HL> up to 00
                                ; saves <BC> & <DE>
        extrn   ?pdec           ; print binary number in <HL> from 0 to 65535
	extrn	?pderr		; print BIOS disk error header
	extrn	?conin,?cono	; con in and out
	extrn	?const		; get console status
	extrn	?sctrn		; sector translation routine
	extrn	@covec

;	status line calls

	extrn	?save,?recov,?stat

; System function call
	extrn	?kyscn
	extrn	?fun65
	extrn	?bank
	extrn	?di$int

	public	?dskst
	public	?dkmov
	extrn	?stat,@st40

	page
;
; Initialization entry point.
; called for first time initialization.
;
	DSEG
init$154X:
	xra	a
	sta	fast
	lxi	h,MFM$table
	shld	MFM$tbl$ptr
	ret


	page
;
; This entry is called when a logical drive is about to
;  be logged into for the purpose of density and type determination.
;  It may adjust the parameters contained in the disk
;  parameter header pointed to by <DE>
;
	DSEG
;
;	if disk type GCR or drive type 1541 or 1581(reports as GCR)
;	   if sector size is 256 bytes
;	      if 1st sector has 'CBM' (1st 3 bytes)
;	         if last byte = -1 (0FFh)
;	            set C128 double sided
;	         else
;	            set C128 single sided
;	         endif
;	      else
;	         set C64 type
;	      endif
;	   else  (512 byte sector size)
;	      set C1581 type
;	   endif
;	else (must be MFM)
;	   TEST MFM
;	endif
;
login$154X:
	call	get$drv$info	; set the drive to check (DPH$pointer set)

	mvi	a,vic$test
				; ***** add code to reset 1581 drive *****
	call	?fun65
	mov	b,a
	ani	0ch
	cpi	0ch		; fast drive ?
	jrz	commodore$type	; no, must be 1541 type
	mov	a,b		; yes, is a 1571 or 1581
	rlc			; MSB=1 if NON-Commodore disk 
	jrc	MFM$type	; 1571 NON-Commodore disk is MFM type

	page
;
;	Commodore Type disk is a disk that is in GCR format (1571)
;	Or Standard Commodore format for 1581 (Has a Commodore dir track)
;
commodore$type:
	lhld	DPH$pointer
	dcx	h
  if	use$1581
	mov	a,b		; get the status byte
	ani	30h		; save only the sector size info
	cpi	20h		; 512 byte sectors?
	jrnz	set$15x1$type	; no, set up as 1571 or 1541
				; yes, set 1581 type drive
;
;
;
set$1581$type:
	mvi	m,dsk$1581	; yes, set up as 1581 double sided
	lxi	d,dpb$1581 
	jr	set$dpb$only

  endif

set$15x1$type:
	mvi	m,dsk$c64
	lxi	d,dpb$c64$cpm	; set DPB to C64
	call	set$dpb$only

	xra	a
	sta	vic$sect	; set track 1 sector 0 (1st sector
	inr	a		; on the disk)
	sta	vic$trk

	lxi	h,@buffer
	shld	local$DMA	; move DMA pointer to disk buffer
	call	login$rd
	ana	a		; read error ?
	rnz			; yes, just return

	RCALL	FR$check$CBM
	rnz			; return if not 'CBM'
				; A=0FFh if double sided
	inr	a
	lhld	DPH$pointer
	dcx	h		; does not affect flags
	mvi	m,dsk$c128

	lxi	d,dpb$c128$SS
	jrnz	set$dpb$only

	lxi	d,dpb$c128$DS

	page
;
;
;
set$dpb$only:
	lxi	b,0		; set sector translation to zero
set$format:
	lhld	DPH$pointer
	mov	m,c
	inx	h
	mov	m,b		; install sector translation
	lxi	b,25-1		; ofset to DPB
	dad	b		; HL points to DPB now
	lxi	b,17		; dpb size
	xchg			; move to DPB location
	ldir
	ret

	page
;
;    TEST MFM()
;	save number bytes/sector
;	   if double sided
;	      mark two sided
;	   endif
;	   find start and end sector numbers
;	   scan table of disk for match (if more then 1 match ask user) 
;
MFM$type:
	mvi	c,01100000b
	ana	c			; A = status(trk1) shifted left 1 
	push	psw			; save in case bad query
	push	b			; save BC

	call	get$max$num$B		; used to set the pointer only
	mov	b,m			; get size, and disk lock flag
	inx	h
	mov	a,m
	inx	h
	mov	h,m			; get last MFM$mactch$ptr
	mov	l,a
	mov	a,b			; get lock flag in A
	ani	80h			; lock bit set ?
	sta	lock$flag		;   (save old lock status)
	shld	last$match		; save last match pointer
	jrz	not$$locked$entry	; yes, then set same disk type
; set$locked$entry
	xra	a
	sta	lock$flag
	mvi	c,0B0h
	lda	vic$data		; get sector size info
	ana	c
	mov	b,a			; save disk sector size info
	xchg				; save HL
	lhld	DPH$pointer
	dcx	h
	mov	a,c
	ana	m			; get old disk sector size
	cmp	b			; are they the same?
	jrnz	not$locked$entry	; no, then unlock disk anyway

	xchg				; get last match pointer (in DE)
	pop	psw			; yes, remove two data elements 
	pop	psw			; ..save on stack
	jr	set$this$entry

not$locked$entry:
	lxi	h,MFM$match$tbl		; clear Match table
	shld	MFM$cur$ptr
	lxi	d,MFM$match$tbl+1
	mvi	m,0
	lxi	b,(MFM$tbl$entries*2)-1+1+1	; table, offset and count
	ldir
	mvi	a,4
	sta	vic$trk			; do query on track 4
	mvi	a,vic$query
	call	?fun65
	pop	b			; recover BC
	ani	0eh			; query error ?
	jrnz	query$error		; yes, use only bits 5 and 6 
	lda	@buffer			; get trk 4 status
	mov	b,a			; save in B
	ani	0eh			; trk 4 status error ?
	jrnz	query$error		; yes, use only bits 5 and 6
	mov	a,b			; recover B (trk 4 status)
	add	a			; shift left
	ana	c			; mask sector size bits
	mov	b,a
	pop	psw			; get trk 1 sector size bits
	cmp	b			; same as trk 4 sector size?
	mvi	c,01111111b
	jrz	trk$1$trk$4		; yes, (then test for mult format)
	mvi	a,80h			; set MSB to mean mult format
	add	b			; ..(track 0 different sector size
					; ..then track 4) 
	mov	b,a			; save in B
	mvi	c,11111111b
trk$1$trk$4:
	lda	@buffer+1		; get number of sectors/track
	sui	4			; remove 4 to extend the range
	add	a			; shift	left
	add	b			; combine with rest of mask
	mov	b,a			; save in B for now

	lda	@buffer+3		; minimum sector number
	add	b			; add in start sector #
	push	psw			; save on stack for a moment

query$error:
	pop	psw			; get value to match 
	ana	c			; test only those bits in the mask

	lhld	MFM$tbl$ptr
	mvi	b,MFM$tbl$entries
check$next:
	push	b			; save BC for a moment
	mov	b,a			; move compare value to 
	mov	a,m			; get type info
	ana	c			; test only the good info
	cmp	b			; match the current type byte
	mov	a,b			;   (recover A)
	pop	b			;   (recover BC)
	jrnz	not$found		; no, do not queue data
					; yes queue table entry address

	xchg				; save adr in DE
	lhld	MFM$cur$ptr
	mov	m,e
	inx	h
	mov	m,d
	inx	h
	shld	MFM$cur$ptr
	lxi	h,MFM$count
	inr	m			; add one to counter
	xchg

	page
;
;
not$found:
	lxi	d,32			; table entry size
	dad	d
	djnz	check$next

	lda	MFM$count		; number of matches in table 
	ana	a			; test for zero
	jz	tell$user$no$entry	; none, tell the user 

	dcr	a			; only one ?
	jrnz	user$select		; no, go check with user (which one)
	lhld	MFM$match$tbl		; yes, use the only one found

;
;	install data from pointer in HL
;
set$this$entry:
	push	h		; save table pointer
	inx	h
	mov	a,m		; get type info. 
	xchg			; save table address in DE
	lhld	DPH$pointer
	dcx	h
	mov	m,a		; save type code
	xchg			; get table adr to HL
	inx	h		; HL points to sector translation table 
	mov	c,m		; ..zero if none
	inx	h
	mov	b,m
	inx	h		; HL points to DPB
	xchg			; DE points to DPB (HL trash)
	call	set$format
	mov	b,m		; get the number of sect/trk from MFM table
	lda	lock$flag	; get the current lock flag value
	ora	b		; combine with sect/trk
	xchg			; HL=to adr,  DE=from adr
	mov	m,a		; install sect/trk and lock flag
	pop	d		; recover table pointer
	inx	h
	mov	m,e
	inx	h
	mov	m,d		; save MFM table pointer at end of DPH
	ret

	page
;
;	let the user select the Disk type (s)he wants
;
user$select:
	inr	a			; number of entries to try to match
	mov	b,a			; set in B as loop count
	lhld	last$match		; get value to match with
	mov	d,h
	mov	e,l			; last match pointer is in DE

	lxi	h,MFM$match$tbl
	shld	MFM$cur$ptr
	mvi	c,0			; start offset at zero

try$next$format:
	mov	a,e
	cmp	m
	inx	h
	jrnz	not$last$match
	mov	a,d
	cmp	m
	jrnz	not$last$match
;
; match, set pointer
;
	mov	a,c			; get offset in A
	push	psw
	call	save$dsk$window
	pop	psw
	jr	set$offset

not$last$match:
	inx	h			; each pointer uses two bytes 
	inr	c			; advance the index
	djnz	try$next$format		; test for more, loop if so

	call	save$dsk$window

	lhld	MFM$cur$ptr
user$loop:
	mov	e,m			; HL=(MFM$cur$ptr)
	inx	h
	mov	d,m
	lxi	h,22			; offset to NAME field
	dad	d			; point to Disk name
	call	dsk$window$old

dsk$user$sel$wait:
	call	?kyscn
	inr	b			; test for key pressed
	jrz	dsk$user$sel$wait
	dcr	b			; adjust back
	mov	a,b			; move matrix position to A
	cpi	SF$exit
	jrnz	CK$dsk$user$rt

	mov	a,c
	ani	4			; control key down ?
	jrz	no$cntr$key		; no, don't lock this selection
	mvi	a,80h			; yes, lock disk type to this drive
no$cntr$key:
	sta	lock$flag		;
	call	dsk$window$remove
	lhld	MFM$cur$ptr
	mov	e,m
	inx	h
	mov	d,m
	xchg
	jr	set$this$entry

	page
;
;
;
CK$dsk$user$rt:
	cpi	SF$right		;
	jrnz	CK$dsk$user$lf

; move window down
	lda	MFM$count		; get number of items in list
	mov	b,a			; save in B
	lda	MFM$offset		; get current position
	inr	a			; advance position
	cmp	b			; at last position ? (n-1+1 =count)
	jrnz	set$offset		; no, then use A as new position
	xra	a			; yes, move back to start
	jr	set$offset
	
CK$dsk$user$lf:
	cpi	SF$left			;
	jrnz	dsk$user$sel$wait

; move window up
	lda	MFM$offset
	dcr	a			; back up offset (under flow?)
	jp	set$offset		; result positive, jump
	lda	MFM$count		; get last item number
	dcr	a			; pointer is 0 to n-1 (not 1 to n)
set$offset:
	sta	MFM$offset		; set new list offset
	inr	a			; add one to adjust for DCR below
	lxi	h,MFM$match$tbl		; set to the beginning

adjust$dsk$loop:
	shld	MFM$cur$ptr		; set pointer here !
	dcr	a			; at offset yet?
	jrz	user$loop		; yes, go display name
	inx	h
	inx	h
	jr	adjust$dsk$loop

	page
;
;
;
tell$user$no$entry:
	lda	vic$data		; get disk test status
	ani	0b0h			; save only sector size and MFM flag
	lhld	DPH$pointer
	dcx	h
	mov	m,a			; set disk size and Type0 (MFM)

	lxi	h,dsk$window*256+buff$pos
	lxi	d,no$dsk$msg
disp$msg$DE$HL:
	call	dsk$window$new
dsk$no$entry$wait:
	call	?kyscn
	inr	b
	jrz	dsk$no$entry$wait
	dcr	b
	mov	a,b
	cpi	SF$exit
	jrnz	dsk$no$entry$wait
;	jr	dsk$window$remove

	page
;
;
;
dsk$window$remove:
	lhld	window$info
	mov	b,h
	mov	c,l
	jmp	?recov
;
;
;
save$dsk$window:
	lxi	h,dsk$window*256+buff$pos	; H=size l=pos
	shld	window$info
	mov	b,h
	mov	c,l
	jmp	?save
;
;
;
dsk$window$new:
	shld	window$info
	xchg
	mov	b,d
	mov	c,e
	push	h
	call	?save
	pop	h

dsk$window$old:
	lda	window$info		; get start index
	inr	a
	mov	c,a			; place in C

dsk$out$next:
	push	h
	lhld	window$info
	mov	a,h
	add	l			; compute max index (start+size)
	dcr	a			; ..less 1
	pop	h
	cmp	c
	rz	
	mov	b,m
	call	dsk$B$out
	inx	h
	jr	dsk$out$next

;
;
;
dsk$B$out:
	mvi	a,01000000b			; set reverse video attr
	push	b
	push	h
	call	?stat				; display space
	pop	h
	pop	b				; recover count
	inr	c
	ret

	page
;
; disk READ and WRITE entry points.
; These entries are called with the following arguments:
;	relative drive number in @rdrv (8 bits)
;	absolute drive number in @adrv (8 bits)
;	disk transfer address in @dma (16 bits)
;	disk transfer bank	in @dbnk (8 bits)
;	disk track address	in @trk (16 bits)
;	disk sector address	in @sect (16 bits)
;       pointer to XDPH in <DE>
;
;   return with an error code in <A>
; 	A=0	no errors
; 	A=1	non-recoverable error
;	A=2	disk write protected
;	A=FF	media change detected
;
	DSEG
read$154X:
	call	get$drv$info
	jm	mfm$rd
	call	set$up$GCR		; compute effective track and sector
login$rd:
	lda	vic$drv
	mov	b,a
	lda	fast			; get fast flags
	ana	b			; isolate fast bit for this drive
	jrnz	rd$fast			; go handle fast drive
rd$slow:
	mvi	a,vicrd			; read a sector of data (A=1)
	call	dsk$fun			; a=0 if no errors
	jnz	test$error		; check for disk error or media change
;
;
;
buf$move:
	xra	a			; set direction to read
	call	?dkmov			; go move buffer to DMA
	lda	sect$cnt
	ana	a
	rz				; a=0 means not read errors
	call	set$up$next
	jr	rd$slow

	page
;
;	A=drive type info
;
mfm$rd:
	call	set$up$MFM

rd$fast:
	mvi	a,vic$rd$f
	call	dsk$fun			; go read the disk

	ani	0eh			; mask off error bits
	jrnz	test$error

	call	get$sector$size
	inr	d
	inr	e			; adjust count for pre-decrement

	call	?di$int
	lxi	b,0DD00h		; D2PRA
	inp	a			; get current clock polarity 
	xri	10h			; toggle clk$bit
	outp	a			; to have status sent (extra clock
					; supplied by rd$1571$data for multi
					; sector transfers)
	lda	vic$count
rd$multi$sect:
	push	psw
	push	d			; save the sector size
	call	rd$1571$data		; read disk data to DMA address
	pop	d

	lda	vic$data
	ani	0eh
	jrnz	test$error$pop		; A=0 if no errors
	pop	psw
	dcr	a
	jrnz	rd$multi$sect
	ei
	lda	sect$cnt
	ana	a			; any sectors left to read
	jrz	done$rd$1571

	call	set$up$next
	jr	rd$fast

done$rd$1571:
	lxi	b,0DD00h		;   D2PRA
	inp	a
	ani	not(10h)		; set clk$bit hi
	outp	a
	xra	a			; A=0 for no errors
	ret


	page
;
;
;
write$154X:
	call	get$drv$info
	jm	mfm$wr
	call	set$up$GCR
	lda	vic$drv
	mov	b,a
	lda	fast			; get fast flags
	ana	b			; isolate fast bit for this drive
	jrnz	wr$fast$drive		; go handle fast drive
wr$slow:
	mvi	a,-1			; set direction to write
	call	?dkmov			; go move DMA to buffer

	mvi	a,vicwr			; write a sector of data
	call	dsk$fun			; a=0 if no errors
	ani	0eh
	jrnz	test$error
	lda	sect$cnt
	ana	a
	rz
	call	set$up$next
	jr	wr$slow

test$error$pop:
	pop	psw
test$error:
	ei
	lda	vic$data
	ani	0fh			; check for disk error or media change
	cpi	0bh			; disk change ?
	jrz	change$error
	cpi	08h			; test for write protect error
	jrz	write$prot$error

	mvi	a,1			;  get general error flag
	ret

;
;
write$prot$error:
	mvi	a,2
	ret

;
;
change$error:
	mvi	a,-1
	ret

	page
;
;
;
mfm$wr:
	call	set$up$MFM
wr$fast$drive:
	mvi	a,vic$wr$f
	call	dsk$fun			; go send the write command

	call	get$sector$size		; setup DMA adr and transfer count
	lda	vic$count
wr$multi$sect:
	push	psw
	push	d			; save sector size
	call	wr$1571$data		; write data to disk from DMA address
	pop	d
	ani	0eh
	jrnz	test$error$pop
	pop	psw
	dcr	a
	jrnz	wr$multi$sect

	ei
	lda	sect$cnt
	ana	a
	rz				; return if no errors (A=0)
	call	set$up$next
	jr	wr$fast$drive

	page
;
;
;
get$drv$info:
	lhld	@dma
	shld	local$dma
	xchg
	shld	DPH$pointer

	lda	@adrv			; get drive number (0 to F)
	ana	a
	cz	drive$A$E
	cpi	'E'-'A'			; test if drive E
	cz	drive$A$E
	dcx	h			; point at drive mask
	dcx	h
	mov	a,m			; get drive mask
	mov	b,a			; save in B
	sta	vic$drv			; save vic drive # (values 1,2,4,8)

	inx	h			; point at disk type
	xra	a
	sta	sect$cnt		; clear the count
	inr	a
	sta	vic$count
	mov	a,m			; get disk type
	ana	a
	ret

;
;	drive A and E share the same physical disk drive (unit 8)
;
drive$A$E:
	mov	b,a
	lda	curdrv			; get the current drive def
	cmp	b			; curdrv = requested drive ?
	rz				; yes, return
					; no, tell the user to swap disk
	push	h
	push	d
	push	b
send$messg:
	mov	a,b			; get requested drive # to A
	sta	curdrv			; make this the current drive
	adi	'A'			; compute drive letter
	sta	msg$drv

	RCALL	FR$bell			; ring BELL to alert user
	lxi	h,swap$msg$lng*256+buff$pos
	lxi	d,swap$msg
	call	disp$msg$DE$HL		; disp and wait for CR

	mvi	a,vic$test
	call	?fun65
;	ani	0fh
;	cpi	0ch			; not fast ERROR ?
;	jrz	exit$drv$A$E		; yes, return that's not a problem
;	ani	0eh			; other error type ?
;	jrnz	send$messg
exit$drv$A$E:
	pop	b
	pop	d
	pop	h
	mov	a,b
	ret

swap$msg:	db	'Insert Disk '
msg$drv:	db	'X in Drive A'

swap$msg$lng	equ	$-swap$msg+2	; +2 for leading and trailing spaces

	page
;	
;
;
get$max$num$b:
	lhld	DPH$pointer
	lxi	b,42			; offset to number of sectors on track
	dad	b
	mov	a,m			; get number sectors/track/side
	ani	1fh
	mov	b,a
	ret
;
;
;
get$sector$size:
	lhld	DPH$pointer
	dcx	h
	mov	a,m			; disk type in B (bit 5,4 size info)
	rrc				; ..00 = 080h byte sectors
	rrc				; ..01 = 100h byte sectors
	rrc				; ..10 = 200h byte sectors
	rrc				; ..11 = 400h byte sectors
	ani	3
	jrz	set$128
	jpo	not$3			; jump if (A=) 01b or 10b
	inr	a			; make A = 4
not$3:
	mvi	e,0			; set E to zero
	mov	d,a			; set sector size (1,2 or 4)
get$DMA:
	lhld	local$DMA		; get the current DMA pointer
	ret

set$128:
	lxi	d,128
	jr	get$DMA 

	page
;
;
;
	DSEG
set$up$GCR:
	cpi	dsk$c128
	jnz	tst$next

	mvi	a,4
	sta	sect$cnt
	lxi	h,sect$buffer
	shld	sect$buf$ptr

	lhld	@trk			; 1 K sector pointer 
	dad	h
	dad	h			; make 256 byte pointer
;
;	build a list of tracks and sectors
;
next$sect:
	shld	@trk
	RCALL	FR$trk$sect
	lhld	vic$trk			; get trk(L) and sector(H) to HL
	xchg
	lhld	sect$buf$ptr
	mov	m,e
	inx	h
	mov	m,d
	inx	h
	shld	sect$buf$ptr
	lhld	@trk
	inr	l			; update saved above at next$sect
	mov	a,l
	ani	3
	jrnz	next$sect
;
;	check list of trk-sectors for number of sectors on this trk
; 
	lxi	h,sect$buffer
	shld	sect$buf$ptr
	lda	vic$drv
	mov	b,a
	lda	fast
	ana	b			; drive type 1571
	jrz	handle$1541		; no, handle as 1541

	lda	sect$cnt		; number of sectors to rd/wr
	mov	b,a
	inx	h
	mov	a,m			; get 1st sector #
	sta	vic$sect
	dcx	h
	mov	a,m			; get 1st track #
	sta	vic$trk

try$next:
	cmp	m			; test for same trk #
	jrnz	exit$no$match
	inx	h
	inx	h			; advance to next trk
	shld	sect$buf$ptr
	djnz	try$next	

exit$no$match:
	lda	sect$cnt		; number of sectors to rd/wr
	sub	b			; remove number left
					; (leaving number matched)
	sta	vic$count		; save number to read
	mov	a,b			; get remaining count
	sta	sect$cnt		; save remaining count
	ret


set$up$next:
	lda	vic$count		; get number of sectors read
	lhld	local$DMA		; get current DMA pointer
	add	h			; advance pointer by number of
	mov	h,a			; sectors read
	shld	local$DMA
handle$1541:
	lhld	sect$buf$ptr
	mov	a,m
	sta	vic$trk
	inx	h
	mov	a,m
	sta	vic$sect
	inx	h
	shld	sect$buf$ptr
	lda	vic$drv
	mov	b,a
	lda	fast
	ana	b
	jrz	set$up$next$slow
	lda	sect$cnt
	sta	vic$count
	xra	a			; two reads max with fast drive
	jr	set$up$next$exit

set$up$next$slow:
	lda	sect$cnt
	dcr	a
set$up$next$exit:
	sta	sect$cnt
	ret
;
;
;
tst$next:
  if	use$1581
	cpi	dsk$1581
	jrz	c1581$adj
  endif
tst$c64:
	mvi	b,dir$track	; set the dir track number
	cpi	dsk$c64		; C64 type disk?
	lda	@sect		;   get sector # to set
	jrz	set$up$c64	; yes, go set up for C64 CP/M disk format
				; no, set up as no type(direct addressing)
;
;	This format is for direct track and sector addressing 
;
do$type$7:
	mvi	b,255		; no dir sector
;
;	this routine will adjust the track number if necessary.
;	The C64 CP/M disk has the C64 directory in the center
;	of the disk. This routine checks and adds one to the track
;	number if we have reached or passed the directory track. 
;
set$up$c64:
	sta	VIC$sect	;
	lda	@trk		;
        cmp     b               ; carry=1 if A < dir$track
	cmc			; add one if dir$track or more (carry not set)
	aci	0		; add the carry bit in
	sta	vic$trk
	ret

  if	use$1581
;
;******	adjust to read multi-512 byte sectors (system sees 1K sector size)
;
c1581$adj:
	mvi	a,2		; 2 512 byte sectors equ 1 1K sector
	sta	vic$count

	lda	@trk		;
        cpi     C1581$dir$trk*2 ; carry=1 if A < dir$track
	cmc			; add one if dir$track or more (carry not set)
	aci	0		; add the carry bit in
	rar			; track=@trk/2 ; carry set if odd
	sta	vic$trk		;

	lda	@sect		; sector # are 0 to 9 (10 sector/trk)
	mov	b,a		;
	jrnc	bottom$1581	;
	adi	80h		; set top of 1581
bottom$1581:
	add	b		; make 0 to 8
	inr	a		; adjust to 1 to 9 (odd numbers only)
	sta	VIC$sect	;
	ret			;

  endif


	page
;
;	A=dsk$info on entry
;
set$up$MFM:
	mvi	d,0		; D=side # (0)
	mov	e,a		; save dsk$info in E
	ani	TypeX		; look at Type0 to Type7
	jrz	do$type$0	;
	cpi	Type2
	lda	@trk		; used by Type1, Type2 and Type3
	jrz	do$type$2
	jrc	do$type$1

;	cpi	Type6
;	jrz	do$type$6
;	jnc	do$type$7	; MSB of sector(byte) set for 2nd side of disk

	cpi	Type7
	jz	do$type$7	; MSB of sector(byte) set for 2nd side of disk
;
;	only types 0 to 2 and 7 are currenty defined
;		Type3 to Type6 will do Type2
;do$type$3:
;do$type$6:

do$type$2:
	mov	b,a		; save a copy in B
	sui	40
	jrc	do$type$0	; jump if still on side 0
	mvi	a,79		; on back side count 39,38,37,...,0
	sub	b
set$trk:
	mvi	d,80h		; D=side # (1)
	sta	@trk
	jr	do$type$0

	page
;
;	divide the track number by two and if Head=1
;		add #sect/side to @sect
;
do$type$1:
	cmc			; carry was set clear it
	rar			; divide track by 2 (carry gets LSB)
	sta	@trk
	jrnc	do$type$0
	call	get$max$num$b	; HL and C changed
	lda	@sect
	add	b
	sta	@sect

do$type$0:
	lda	@trk
	sta	vic$trk
	call	get$max$num$b	; B=number of sectors per track per side
	lda	@sect		; ..HL and C changed
	cmp	b
	jrc	is$side$0
	mvi	d,80h		; D=side # (1)
	bit	C1$bit,e	; dsk$info in E
				; sector numbering continues on side 1 ?
	jrnz	is$side$0	; yes, do not remove side one bias
	sub	b		; no, remove side one bias
is$side$0:
	mov	c,a		; hold @sect in C	
	mov	a,e		; get dsk$info to A
	ani	S1		; A=Starting  sector number (0 or 1)
	add	c		; add back @sect
	ora	d		; add in the side bit
	sta	vic$sect
	ret

	page
;
;	input:
;		DE = number bytes to read
;		HL = DMA address
;
	CSEG
rd$1571$data:
	lda	@dbnk			; get the disk DMA bank
	call	?bank			; set it

	lxi	b,0DC0Dh		; D1ICR
rd$1571$stat$wait:
	inp	a
	ani	8			; data ready bit set?
	jrz	rd$1571$stat$wait	; no, loop

	mvi	c,0ch			; D1SDR
	inp	a			; read the status byte
	sta	vic$data		; save it
	ani	0eh			; any errors ?
	jrnz	rd$1571$exit		; yes, exit

	lxi	b,0DD00h
	inp	a			; get current clock polarity

rd$1571$next:
	lxi	b,0DD00h		; D2PRA
	xri	10h			; toggle clk$bit
	outp	a			; clock the 1571 for a byte

	dcr	e			; DE=count
	jnz	rd$1571$more		; leave as normal jump to keep
	dcr	d			; the transfer speed at it's max
	jrz	rd$1571$exit		; ...

;
rd$1571$more:
	dcr	b
rd$1571$wait:
	mvi	c,0dh			; D1ICR (DC0Dh)
	inp	c
	bit	3,c
	jz	rd$1571$wait
	mvi	c,0ch			; D1SDR
        ini                             ; (hl) <- (bc) ; hl <- hl+1 ; b <- b-1
	jmp	rd$1571$next


rd$1571$exit:
	sta	bank$0			; restore current mem config
	ret

	page

clk$in	equ	40h
;
;	input:
;		DE = number of bytes to write
;		HL = DMA address
;
wr$1571$data:
	call	?di$int
; do spout inline
	lxi	b,mode$reg
	mvi	a,fast$wr$en
	sta	io$0
	outp	a			; set data direction to output
	sta	bank$0

	lxi	b,0dc05h		; low (D1T1h)
	xra	a
	outp	a
	dcr	c			; low(D1T1l)
	mvi	a,3			; clk = osc/3
	outp	a			;

	mvi	c,0eh			; D1CRA
	inp	a
	ani	80h
	ori	55h
	outp	a
	dcr	c			; D1ICR
	inp	a

	lda	@dbnk			; get the disk DMA bank
	call	?bank			; set it

	mvi	a,clk$in
	sta	cur$clk

	page
;
;
clk$wait:
	lxi	b,0dd00h		; D2PRA
	inp	a
	inp	c			; debounce
	cmp	c
	jrnz	clk$wait

	lda	cur$clk			; get old clk value
	xra	c			; check if changed 
	ani	clk$in			; (only clock in bit)
	jrz	clk$wait		; loop if not

	mov	a,c			; 
	sta	cur$clk			; make this the current clk value
	lxi	b,0dc0ch		; D1SDR
	mov	a,m
	outp	a			; send character to drive
	inx	h			; advance pointer
	dcx	d			; dec the char count

	inr	c			; D1ICR
send$wait:
	inp	a
	ani	8
	jz	send$wait

	mov	a,d
	ora	e
	jnz	clk$wait		; go send the next byte

; do spin
	lxi	b,0DC0Eh		; D1CRA
	inp	a
	ani	80h
	ori	8
	outp	a
	lxi	b,mode$reg
	mvi	a,fast$rd$en
	sta	io$0			; enable the MMU
	outp	a			; set data direction to input
	sta	bank$0			; disable MMU
; spin done

	page

	lxi	b,0DC0Dh		; D1ICR
	inp	a			; clear data pending flag

	lxi	b,0DD00h		; D2PRA
	inp	a
	ori	10h			; set clk$bit low (hardware inverted)
	outp	a			; 

	lxi	b,0DC0Dh		; D1ICR
wait$status:
	inp	a
	ani	8
	jrz	wait$status

	lxi	b,0DC0Ch		; D1SDR
	inp	d

	lxi	b,0DD00h		; D2PRA
	inp	a
	ani	not(10h)		; set clk$bit hi (hardware inverted)
	outp	a			; 

	mov	a,d			; recover the status byte
	sta	vic$data

	ei
	ret

	page
;
;	This routine is used to move a sector of data
;	 to/from the sector buffer and the DMA pointer.
;	     A=0 for buffer to DMA  (disk read)
;            A<>0 for DMA to buffer (disk write)
;
	CSEG
?dkmov:
	lhld	local$DMA	; current DMA adr
	lxi	d,@buffer	; location of disk read/write buffer
	lxi	b,256		; sector size
;
;
dk$cont:
	ora	a
	jrnz	dsk$read	; swap pointer for read
	xchg
;
;
dsk$read:
	lda	@dbnk		; get the disk bank
	call	?bank
	ldir			; do the data move
	sta	bank$0		; current bank will ALWAYS be 0
	ret

;
;
;
	DSEG
dsk$fun:
	sta	vic$cmd
	lda	stat$enable
	ani	1			; display of disk info enabled?
	cnz	disp$dsk$info		; yes, go display disk info
	jmp	?fun65+3		; go do the function

	page
;
;
;
	DSEG
?dskst:
disp$dsk$info:
	mvi	a,72			; r/w in col 72 (col 0-79)
	sta	offset
	lda	vic$cmd
	mvi	b,'R'
	dcr	a			; ?1 normal$rd
	jrz	out$cmd$rd
	dcr	a			; ?2 normal$wr
	jrz	out$cmd$wr
	dcr	a			; ?3 fast$rd
	jrz	out$cmd$rd
	dcr	a			; ?4 fast$wr
	rnz
out$cmd$wr:
	mvi	b,'W'
out$cmd$rd:
	call	disp$B
	call	disp$space
	mvi	b,'A'-1
	lda	vic$drv
next$drv:
	inr	b
	rrc
	jrnc	next$drv

	call	disp$B
	lda	vic$trk
	call	disp$dec
	lda	vic$sect
	ani	80h
	cz	disp$space
	mvi	b,'-'
	cnz	disp$B
	lda	vic$sect	
	ani	7fh

	page
;
;
;
disp$dec:
	mvi	b,'0'-1

conv$loop:
	inr	b
	sui	10
	jrnc	conv$loop

	adi	'0'+10
	push	psw
	call	disp$B
	pop	psw
disp$A:
	mov	b,a
disp$B:
	lxi	h,@st40-72+40-8
	lda	offset
	mov	e,a
	mvi	d,0
	dad	d			; add the offset
	mov	m,b			; save on 40 col display

	mov	a,e
	mov	c,a			; col # in C
	inr	a
	sta	offset			; advance cursor position
	xra	a			; no attribute to write
	call	?stat

	lxi	h,@st40
	lxi	d,vic$screen+40*24	; update 40 column screen
	lxi	b,40
	ldir
	xra	a
	ret

disp$space:
	mvi	b,' '
	jr	disp$B

	page
;
; Extended Disk Parameter Headers (XDPHs)
;
	CSEG			; place tables in common
;
;	1st disk drive on the system
;
	dw	write$154X
	dw	read$154X
	dw	login$154X
	dw	init$154X
	db	1		; bit 0 set (drive 0)
	db	dsk$c128	; format type byte
cmdsk0:	
	dph	0,dpb$0

dpb$0:
	dpb	1024,5,159,2048,128,0
	db	0		; max sector number and lock flag
	dw	0		; MFM table pointer

	page
;
;	2nd disk Drive on the system
;
	dw	write$154X
	dw	read$154X
	dw	login$154X
	dw	init$154X
	db	2		; bit 1 set (drive 1)
	db	dsk$c128	; format type byte 
cmdsk1:
	dph	0,dpb$1

dpb$1:
	dpb	1024,5,159,2048,128,0
	db	0		; max sector number and lock flag
	dw	0		; MFM table pointer

	page
;
;	3rd disk drive on the system
;
	dw	write$154X
	dw	read$154X
	dw	login$154X
	dw	init$154X
	db	4		; bit 2 set (drive 2)
	db	dsk$c128	; format type byte
cmdsk2:	
	dph	0,dpb$2

dpb$2:
	dpb	1024,5,159,2048,128,0
	db	0		; max sector number and lock flag
	dw	0		; MFM table pointer

	page
;
;	4th disk drive on the system
;
	dw	write$154X
	dw	read$154X
	dw	login$154X
	dw	init$154X
	db	8		; bit 3 set (drive 3)
	db	dsk$c128	; format type byte 
cmdsk3:
	dph	0,dpb$3

dpb$3:
	dpb	1024,5,159,2048,128,0
	db	0		; max sector number and lock flag
	dw	0		; MFM table pointer

	page
;
;	Drive E: shared with 1st drive (A:)
;
	dw	write$154X
	dw	read$154X
	dw	login$154X
	dw	init$154X
	db	1		; bit 0 set (drive 0)
	db	dsk$c128	; format type byte 
cmdsk4:
	dph	0,dpb$4

dpb$4:
	dpb	1024,5,159,2048,128,0
	db	0		; max sector number and lock flag
	dw	0		; MFM table pointer

	page
;
;	NOTE: The blocking factor for all of these formats is
;		1K (2K for double sided), thus the fractional
;		parts are unusable by CP/M.  They can be accessed
;		by absolute sector addressing.
;
;	NOTE: 1571 and 1541 disk drives use track numbers
;		of 1 to 35 and sector numbers of 0 to nn

;
;		The method used to access the full disk
;		is to tell the system that there is 1 sector
;		per track and then to use the track # as an
;		absolute sector address and do conversion in BIOS.
;
; 
; DPB FOR C128 CP/M 3.0 disk		( 170K, 34K larger then C64 CP/M)
;	256 byte sectors		( 170.75K )
;	1 sectors/track
;		up to 21 physical sectors (0 to 16,17,18 or 20)
;	680 tracks/disk (usable, 683 real)
;		35 physical tracks (0 to 34)
;	1K allocation blocks
;	64 directory entries
;	track offset of 0
;
	DSEG		; these tables are moved to common when used
dpb$c128$SS:		; (170 allocation units)
	dpb	1024,1,170,1024,64,0

	page
;
; DPB FOR C128 CP/M 3.0 double sided disk	( 340K )
;	1024 byte sectors (phy=256)		( 341.5K )	
;	1 sectors/track
;		up to 21 physical sectors (0 to 16,17,18 or 20)
;	340 tracks/disk (usable, 1366 real)
;		70 physical tracks (0 to 34 side 0, 35 to 69 side 1)
;	2K allocation units
;	128 directory entrys
;	track offset of 0
;
dpb$c128$DS:		; (170 allocation units)
	dpb	1024,1,340,2048,128,0

	page
;
; DPB FOR C64 CP/M 2.2 disk -- 			( 136K )
;	256 byte sectors
;	17 sectors / tracks	(sector numbering 0-16)
;		sector 18 to n on the outer tracks are unused
;	34 tracks / disk
;		tracks track 2 to 16    (track numbering 0-34)
;		track 17 is the C128 directory track (not counted)
;		track 19 to 34
;	1K allocation blocks
;	64 directory entrys
;	track offset of 3 (1st two tracks used for CP/M 2.2 boot) plus
;	one sector to adjust for sector numbering of 1 to 35 (not 0 to 34)
;
dpb$c64$cpm:		; (144 allocation units)
	dpb	256,17,34,1024,64,3
                
	page
;
; DPB FOR C128 CP/M 3.0 C1581 DSDD (3.5")	(    K )
;	512 byte sectors			( 720K )
;	10 sectors/track
;	159 tracks/disk
;		160 physical tracks 80 on top, 79 on bottom, 1 used for
;		BAM and disk directory (1581 DOS) (10 sectors per track)
;	2K allocation units
;	128 directory entrys (2 allocation units)
;
  if	use$1581
dpb$1581:		; (xxx allocation units)
	dpb	1024,5,159,2048,128,0
  endif

	page
;
	DSEG
MFM$table:
	db	S256*2+(16*2-8)+1	; 256 byte sect, 16 sect/trk
	db	MFM+S256+Type0+C0+S1	; 	DSDD
	dw	0			; start on track 2 sect 1 (2 alc)
	dpb	256,32,40,2048,128,2	; sect# 1 to 16
	db	16			; (top and bottom numbered the same)
	db	'Epson QX10'		;1 Epson QX10
					; 160 allocation units




	db	80h+S512*2+(10*2-8)+1	; 512 byte sect, 10 sect/trk
;	db	S256*2			; track 0 is 256 bytes/sector
	db	MFM+S512+Type0+C0+S1	;	DSDD
	dw	0			; start on track 2 sect 1 (2 alc)
	dpb	512,20,40,2048,128,2	; sect# 1 to 10
	db	10			; (top and bottom numbered the same)
	db	'Epson QX10'		;2
					; 200 allocation units

	page

	db	S512*2+(8*2-8)+1	; 512 byte sect 8 sect/trk
	db	MFM+S512+Type2+C0+S1	; 	SSDD
	dw	0			; start on track 1 sector 1 (2 alc)
	dpb	512,8,40,1024,64,1	; sect# 1 to 8
	db	8			;
	db	' IBM-8 SS '		;3
					; 160 allocation units




	db	S512*2+(8*2-8)+1	; 512 byte sect 8 sect/trk
	db	MFM+S512+Type2+C0+S1	; 	DSDD
	dw	0			; start on track 1 sector 1 (1 alc)
	dpb	512,8,80,2048,64,1	; sect# 1 to 8
	db	8			; (top and bottom numbered the same)
	db	' IBM-8 DS '		;4
					; 160 allocation units

	page

	db	S512*2+(10*2-8)+0	; 512 byte sector, 10 sect/trk
	db	MFM+S512+Type1+C1+S0	;	DSDD
	dw	0			; start on track 0 sector 10 (2 alc)
	dpb	512,10,80,2048,128,1	; sect# 0 to 9 on top (even tracks)
	db	10			; sect# 10 to 19 on bottom (odd tracks)
	db	'KayPro IV '		;5
					; 200 allocation units




	db	S512*2+(10*2-8)+0	; 512 byte sect, 10 sect/trk
	db	MFM+S512+Type0+C1+S0	; 	SSDD
	dw	0			; start on track 1 sector 0 (4 alc)
	dpb	512,10,40,1024,64,1	; sect# 0 to 9 
	db	10			;
	db	'KayPro II '		;6
					; 200 allocation units

	page

	db	S1024*2+(5*2-8)+1	; 1024 byte sect, 5 sect/trk
	db	MFM+S1024+Type0+C0+S1	; 	SSDD
	dw	0			; start on track 3 sector 1 (2 alc)
	dpb	1024,5,40,1024,64,3	; sect# 1 to 5
	db	5			;
	db	'Osborne DD'		;7
					; 200 allocation units


	db	S512*2+(9*2-8)+1	; 512 byte sect 9 sect/track (uses 8)
	db	MFM+S512+Type1+C0+S1	; 	DSDD
	dw	0			; start on trk 0, sect 1, hd 1 (1 alc)
	dpb	512,8,80,2048,64,1	; sect# 1 to 9
	db	8			; (top and bottom numbered the same)
	db	'  Slicer  '		;8
					; 160 allocation units

	page

	db	S256*2+(16*2-8)+1	; 256 byte sect, 16 sect/trk
	db	MFM+S256+Type0+C0+S1	; 	DSDD
	dw	0			; start on track 4 sect 1 (2 alc)
	dpb	256,32,40,2048,128,4	; sect# 1 to 16
	db	16			; (top and bottom numbered the same)
	db	'Epson Euro'		;9 Epson European (MFCP/M ?)
					; 160 allocation units



	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;10

	page

	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;11

	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;12

	page

	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;13

	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;14

	page

	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;15

	db	-1
	db	MFM			; 
	dw	0			; 
	dpb	512,20,40,2048,128,2	; 
	db	8			;
	db	'   None   '		;16


	page
;
;	not functional yet
;

;	db	S1024*2+(5*2-8)+1	; 1024 byte sect 5 sect/track
;	db	MFM+S1024+Type0+C0+S1	; 	SSDD
;	dw	0			; start on trk 2, sect 1 (2 alc)
;	dpb	1024,5,40,2048,128,2	; sect# 1 to 5
;	db	5			;
;	db	'Morrow MD2'		; 





;	db	S1024*2+(5*2-8)+1	; 1024 byte sect  5 sect/trk
;	db	MFM+S1024+Type0+C0+S1	; 	DSDD
;	dw	0			; start on trk 1, sect 1, hd 0 (3 alc)
;	dpb	1024,10,40,2048,192,1	; sect# 1 to 5
;	db	5			;
;	db	'Morrow MD3'		; 


MFM$tbl$entries	equ	($-MFM$table)/32

	db	-1			; mark end of table
	db	-1

	page

	cseg
cur$clk:	ds	1

	dseg
lock$flag	ds	1
last$match	ds	2
window$info:	ds	2

dsk$window	equ	12

no$dsk$msg:
		;1234567890
	db	' Missing  ' 


MFM$match$tbl:
	ds	2*MFM$tbl$entries	; MFM$count MUST follow this parm
MFM$count:
	ds	1			; MFM$offset MUST follow this parm
MFM$offset:
	ds	1

MFM$cur$ptr:
	ds	2

DPH$pointer:
	ds	2

sect$cnt:
	ds	1
sect$buf$ptr:
	ds	2
sect$buffer:
	ds	4*2

local$DMA:
	ds	2

status$atr	equ	0
offset:		db	0

	end
