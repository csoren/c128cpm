
	title	'C128 keyboard handler   18 Feb 86'

	maclib	cxequ

	maclib	z80

	public	?get$key,?int$cia,?kyscn
	public	Fx$V$tbl

	extrn	?stat,?save,?recov
	extrn	?dskst

	extrn	?fun65

	extrn	cmdsk0,cmdsk1,cmdsk2,cmdsk3,cmdsk4

	extrn	@pageM

	extrn	adm31
;	public	setadm

  if	use$VT100
	extrn	vt100
	public	setvt
  endif

	page

	DSEG
;
;
;
?int$cia:
	lxi	b,key$row		; point to CIA 1st register
	mvi	a,0ffh
	outp	a
	inr	c
	inr	c
	outp	a
	inr	c
	xra	a
	sta	commodore$mode		; clear commodore shift mode
	outp	a


	lxi	h,key$scan$tbl		; init key scan tbl pointer
	mov	m,l			; ..to the begining
;
;	initialize keyboard buffer and pointers
;
	lxi	h,key$buffer
	shld	key$get$ptr
	shld	key$put$ptr
	mvi	m,0ffh
	lxi	d,key$buffer+1
	lxi	b,key$buf$size-1
	ldir
	lhld	key$tbl
	lxi	d,-(8*4*8)
	dad	d
	shld	gkey$tbl
	ret

gkey$tbl:
	dw	0

	page
;==========================================================
;		KEYBOARD SCANNING FUNCTION
;==========================================================
;
;
;
;
?get$key:
	lhld	msgptr
	mov	a,h
	ora	l
	jrnz	mess$cont

;
;
;
re$scan:
	call	scan$keys
	push	psw

	mov	a,c
	cma
	ani	special
	jnz	not$special

	mov	a,b		; get the matrix position
	cpi	rt$arrow
	jz	prog$fun

	cpi	lf$arrow
	jz	prog$key

	cpi	alt$key
	jz	toggle$plain$keys

;
;
;
not$special:
	pop 	psw
	rp
	mov	d,a
	lda	stat$enable
	ani	80h		; mask off plain keys bit
	mov	a,d		; recover input character
	rnz			; return if plain keys bit is set

	page
;
;
;
test$function:
	ora	a		; check for MSB set 
	rp			; return if not

	cpi	0A0h		; 80-9F are function keys
	jrnc	not$8x

;
;
find$mess:
	ani	1fh		; 32 messages
	mov	b,a		; place Function # in B for search
	call	get$fun$adr

;
;
mess$cont:
	mov	b,m		; get char to B
	inx	h
	mov	a,m
	ora	a
	jrnz	more$mess

	lxi	h,0

more$mess:
	shld	msg$ptr
	mov	a,b
	mvi	c,0		; no control keys
	mvi	b,0f0h		; tell user this is a function key
	ora	a		; check character (maybe 1st is 0)
	jrz	re$scan		; scan keys (no valid function key)

	jr	test$function	; test for local function

	page
;
;
;
get$fun$adr:
	lhld	fun$tbl			; get adr of msg table
	dcx	h
; lxi	h,msgtbl-1			; point to start of funtions (less one)
	inr	b			; adjust function # (to test for 0)
	xra	a			; get a zero in A
check$fun$num:
	inx	h			; advance pointer to point at text
	shld	msg$ptr			; save message adr for caller
	dcr	b			; requested function ?
	rz				; yes, exit with HL=string adr 

find$end$marker:
	cmp	m			; end of text marker ? (0=EOTM)
	jrz	check$fun$num		; yes, go see if required fun # 

	inx	h			; advance to next char
	jr	find$end$marker		; go find EOTM

	page
;
;	A0-AF	Set char color (80 col)
;	B0-B1	Set background color (80 col)
;
not$8x:
	cpi	0C0h		; 
	jrnc	not$80col$color

	sui	0A0h-20h		; remove key bias
	mov	b,a
	RCALL	FR$color
	jr	?get$key

;
;	C0-CF	Set char color (40 col)
;	D0-DF	Set background color (40 col)
;	E0-EF	Set border color (40 col)
;
not$80col$color:
	cpi	0F0h
	jrnc	must$be$Fx
;
;
;
	sui	0C0h-20h		; remove key bias
	mov	b,a
	RCALL	FR$color+FR$40
	jr	?get$key

	page
;
;	F0-FF	special code functions
;               		    
must$be$Fx:
	lxi	h,?get$key
	push	h			; save as the return adr
	ani	0fh
	add	a			; double
	lhld	key$FX$function
	mov	e,a
	mvi	d,0
	dad	d			; HL points to the function
	mov	e,m
	inx	h
	mov	d,m
	xchg
	pchl

;
;
;
FX$V$tbl:
	dw	toggle$dsk$stat		; F0
	dw	display$pause		; F1
	dw	empty			; F2
	dw	cur$lf			; F3
	dw	cur$rt			; F4
	dw	reset$mfm		; F5
  if	use$VT100
	dw	set$adm			; F6
	dw	set$VT			; F7
  else
	dw	empty			; F6
	dw	empty			; F7
  endif
	dw	empty			; F8
	dw	empty			; F9
	dw	empty			; FA
	dw	empty			; FB
	dw	empty			; FC
	dw	empty			; FD
	dw	empty			; FE

	dw	0			; FF	go restart the C128 BASIC
					;	mode (or C64)


;	dw	screen$print$40		; would be nice later
;	dw	screen$print$80

	page
;
;	Function F0
;
toggle$dsk$stat:
	lda	stat$enable
	xri	1
	sta	stat$enable

	ani	1
	jnz	?dskst			; go paint the disk status line

;
;	erase 80 column window from display
;
	mvi	e,8
	lxi	b,20h*256+(80-8)	; get a space start in col 80-8	
erase$loop:
	push	d			; save count
	push	b			; save space and position
	xra	a			; get no attributes
	call	?stat			; update screen
	pop	b			; recover space and position
	inr	c			; advance position
	pop	d			; recover count
	dcr	e			; decrement count
	jrnz	erase$loop		; loop until done

;
;	erase 40 column window from display
;
	RJMP	FR$screen$paint

	page
;
;	Function F1
;
display$pause:
	mvi	a,-1
	sta	cur$pos			; move cursor out of window

	lxi	b,buff$small*256+buff$pos	; B=size C=pos
	call	?save
	mvi	a,buff$pos+1
	sta	offset
	mvi	b,5
	lxi	h,pause$MSG

pause$disp$loop:
	mov	a,m
	push	h
	push	b
	call	disp$status
	pop	b
	pop	h
	inx	h
	djnz	pause$disp$loop

pause$loop:
	call	scan$keys
	jrz	pause$loop
	cpi	0f1h			; pause key function code
	jrnz	pause$loop
	jmp	recov$small


pause$MSG:
	db	'Pause'


	page
;
;	Function F2
;
;	A Zero in bit 6 of STAT$ENABLE will enable tracking
;	the cursor on data input with the 40 column display
;
;toggle$track$40:
;	lda	stat$enable
;	xri	40h
;	sta	stat$enable
;empty:
;	ret



	page
;
;	Function F3
;
;	Move 40 column window left one half screen
;
cur$lf:
	lda	@off40
	ora	a
	rz

	sui	20
	jr	cur$update$cont







;
;	Function F4
;
;	Move 40 column window right one position
;
cur$rt:
	lda	@off40
	cpi	40
	rz
	adi	20
cur$update$cont:
	sta	@off40
	RCALL	FR$set$cur$40
	RJMP	FR$screen$paint

;
;	Function F5
;
;	Unlock MFM selection for ALL drives in the system
;
reset$mfm:
	lda	cmdsk0+42		; 42 is the offset from drive pointer
	ani	7fh			; MSB cleared to unlock the drive
	sta	cmdsk0+42		; unlock drive A
	lda	cmdsk1+42
	ani	7fh
	sta	cmdsk1+42		; unlock drive B
	lda	cmdsk2+42
	ani	7fh
	sta	cmdsk2+42		; unlock drive C
	lda	cmdsk3+42
	ani	7fh
	sta	cmdsk3+42		; unlock drive D
	lda	cmdsk4+42
	ani	7fh
	sta	cmdsk4+42		; unlock drive E
empty:
	ret

;
;	Function F6
;
  if use$vt100
set$adm:
	lxi	h,ADM31
	jr	set$emulation
;
;	Function F7
;
set$VT:
	lxi	h,VT100
set$emulation:
	shld	emulation$adr
	ret
  endif
;
;
;	THIS CODE IS NOT FUNCTIONAL YET
;
;toggle$page$break:
;	lda	@pageM
;	xri	0ffh
;	sta	@pageM
;	rz
;	mvi	a,-1
;	sta	@pageM


	page
;
;	A zero in the MSB of the STAT$ENABLE byte will allow
;	special keyboard function. (codes above 80h)
;	A one will force the key value to be returned without
;	any special functions being executed.
;
toggle$plain$keys:
	pop	psw			; remove garbage

	lda	stat$enable
	xri	80h
	sta	stat$enable
	jmp	re$scan

	page
;
;
;
prog$key:
	pop	psw			; remove garbage

	lxi	b,buff$small*256+buff$pos	; B=size, C=position
	call	?save

	mvi	a,buff$pos+1
	sta	offset
	call	read$key		; get key to re-program
	push	h			; save key's address
	mov	a,m
	call	disp$hex$byte
	mvi	a,buff$pos+4
	sta	offset
	call	get$byte
	pop	h
	jrc	restore$buf$small
	mov	m,a
;
;
restore$buf$small:
	call	delay
recov$small:
	lxi	b,buff$small*256+buff$pos	; B=size, C=position
	jmp	?recov

	page
;
;
;
prog$fun:
	pop	psw			; remove garbage

	lxi	b,buff$large*256+buff$pos	; b=size, c=pos
	call	?save

	call	read$key		; get function key to program
	cpi	80h
	jrc	restore$buf$large	; error, exit

	cpi	0A0h
	jrnc	restore$buf$large

	ani	1fh			; 32 keys defined
	mov	b,a
	call	get$fun$adr		; get pointer to function code

	xra	a
	sta	string$index		; start at start of string

	call	edit$fun

	lxi	h,0
	shld	msg$ptr			; clear message pointer

restore$buf$large:
	call	delay
	lxi	b,buff$large*256+buff$pos	; B=size, C=position
	jmp	?recov

	page
;
;
;
delay:
	lxi	h,0
delay$loop:
	dcx	h
	mov	a,h
	ora	l
	jrnz	delay$loop
	ret
;
;
;
edit$fun:
	lxi	h,edit$fun
	push	h			; set return to here
	call	disp$fun$key
	call	read$key		; B=matrix position
	mov	d,a			; save ASCII char in D
	mov	a,c			; get attr (C=cntr codes)
	ani	special
	cpi	special			; check for cntr shift
	jnz	not$cntr$shift


;
;
;
check$exit:
	mov	a,b			; get matrix position
	cpi	SF$exit
	jrnz	check$delete

	pop	h			; remover return adr
	ret				; go back to normal keyboard fun

	page
;
;
;
check$delete:
	cpi	SF$delete
	jrnz	check$insert
;
;	delete the character at current cursor position
;
	call	compute$adr		; HL= current position
	rz				; don't want to delete end markers

	xchg				; save in DE
	lhld	gkey$tbl		; get next table adr (gkeytbl)
	dcx	h
;       lxi     h,msgtbl$end-1          ; end adr
	xra	a			; clear the carry flag
	dsbc	d			; compute number of bytes to move
	mov	b,h
	mov	c,l			; place count in BC
	mov	h,d
	mov	l,e			; HL=DE
	inx	h			;
	ldir

	dcx	h			; point to insert point
	mvi	m,-1			; fill table end with -1
	ret

	page
;
;
;
check$insert:
	cpi	SF$insert
	jrnz	check$right
;
;	insert a space into string
;
	call	compute$adr
;
;	HL=address to insert a space at
;	 value of HL is the same on return
;
insert$space:
	xchg
	lhld	gkey$tbl		; get start of next table
	dcx	h			; point to end of msg table
; lxi	h,msgtbl$end-1
	xra	a
	cmp	m			; last char=0 (end of string)
	rz				; yes, don't insert 

	xra	a			; clear the carry flag
	dsbc	d			; compute number of bytes to move
	mov	b,h
	mov	c,l			; place count in BC

	lhld	gkey$tbl
	dcx	h
	mov	d,h
	mov	e,l
; lxi	d,msgtbl$end-1			; dest adr
	dcx	h
; lxi	h,msgtbl$end-2			; source adr
	lddr				; move the data
	inx	h			; point to insert point
	adi	' '			; A was equ to zero, add a space to
	mov	m,a			; ..clear the zero flag
	ret				; insert a space at the new location

	page
;
;
;
check$right
	cpi	SF$right
	jrnz	check$left
;
;	move cursor right
;	 if past right end go back to left end
;
	call	compute$adr
	lda	string$index
	jrnz	move$rt

	mvi	a,-1	
move$rt:
	inr	a
	sta	string$index
	ret

;
;
;
check$left:
	cpi	SF$left
	rnz
;
;	move cursor left
;	 if past left end go to right end
;
	lda	string$index
	ora	a
	jrz	at$left$end

	dcr	a
	sta	string$index
	ret


	page
;
;
;
at$left$end:
	call	compute$adr
	rz				; return if at right end

	lda	string$index
	inr	a
	sta	string$index		; move right one position
	jr	at$left$end		; 



;
;
;
not$cntr$shift:
	call	compute$adr		; HL=function adr (A=0 if string end)
	jrnz	no$insert

	push	d			; save char to insert
	call	insert$space
	pop	d			; recover character
	rz				; no room if zero flag set

no$insert:
	mov	m,d			; install key's value
	lda	string$index
	inr	a
	sta	string$index
	ret

	page
;
;
;
compute$adr:
	lhld	msg$ptr			; get start of memory pointer
	lda	string$index		; get current offset
	add	l
	mov	l,a
	mov	a,h
	aci	0
	mov	h,a			; point to update location
	mov	a,m
	ora	a
	ret

;
;
;
disp$fun$key:
	mvi	a,buff$pos
	sta	offset
	mvi	a,'>'			; display start prompt '>'
	call	disp$status

	lhld	msg$ptr
	lda	string$index

try$again:
	cpi	buff$large-2
	jrc	parameters$ok

	inx	h
	dcr	a
	jr	try$again

	page
;
;
;
parameters$ok:
	adi	buff$pos+1
	sta	cur$pos 

disp$fun$loop:
	mov	a,m
	ora	a
	inx	h	 		; advance function pointer
	jrz	disp$fun$end

	push	h
	call	disp$status		; display on status line
	pop	h
	lda	offset			; get current cursor position
	cpi	buff$pos+buff$large-1	; to end of window?
	jrnz	disp$fun$loop		; no, display next character


disp$fun$end:
	mvi	a,'<'			; display end prompt '<'
disp$space$fill:
	call	disp$status
	lda	offset			; get current cursor position
	cpi	buff$pos+buff$large	; to end of window?
	rz

	mvi	a,' '			; fill to the end with spaces
	jr	disp$space$fill

	page
;
;
;
disp$hex$byte:
	push	psw
	rar
	rar
	rar
	rar
	call	disp$hex$nibble
	pop	psw

disp$hex$nibble:
	ani	0fh
	adi	'0'
	cpi	'9'+1
	jrc	disp$status

	adi	7

disp$status:
	mov	b,a
	lda	offset
	mov	c,a
	inr	a
	sta	offset
	lda	cur$pos
	cmp	c
	mvi	a,01000000b		; set reverse video attributes
	jrnz	not$cur$pos

	mvi	a,00010000b		; set normal video and blink
not$cur$pos:
	jmp	?stat


	page
;
;
;
get$byte:
	mvi	e,0
	call	read$nibble
	rc

	add	a
	add	a
	add	a
	add	a
	mov	e,a

read$nibble:
	push	d
	call	read$key
	mov	a,b			; get matrix position
	lxi	h,hex$key$tbl
	lxi	b,16
	ccir

	mov	a,c
	pop	d
	stc
	rnz

	add	e
	push	d
	push	psw
	call	disp$hex$nibble
	pop	psw
	pop	d
	stc
	cmc
	ret

;
;
;
read$key:
	call	scan$keys
	inr	b
	jrz	read$key		; no, wait for one
	dcr	b
	ret

	page
;
;
;
do$alpha$toggle:
	mvi	m,0ffh		; mark buffer position free	
	lda	commodore$mode
	xri	00100001b
	ani	00100001b
	sta	commodore$mode
;	
; output:
;	B=FF if no key pressed
;	A=00 if no key code assigned
;	else 	A=ASCII key code 
;		B=matrix position (0-57)
;		C=control code (bits 1,0)
;			00=lower case	(lowest)
;			01=upper case
;			10=shift
;			11=control	(highest)
;		  (bit 2) control key
;		  (bit 4) rt. shift key
;		  (bit 5) commodore key
;		  (bit 7) lf. shift key
;
;	HL= address of ASCII key location
;
?kyscn:
scan$keys:
	lhld	key$get$ptr
	mov	a,m		; M=-1 if buffer empty
	mov	b,a		; B=-1 if no character
	inr	a
	jrnz	is$key		; go on
	lxi	h,caps$count
	inr	m
	jrz	test$caps
	ora	a
	ret
caps$count:
	db	0

;
;	there is a character in the buffer,
;	advance key$get$ptr to next character.
;
is$key:
	mov	a,l
	adi     2
	cpi     low(key$buffer+key$buf$size)
	jrnz	not$buf$end
	mvi	a,low(key$buffer)
not$buf$end:
	sta	key$get$ptr	; update low byte of pointer

	page
;
;	test for commodore key, if found toggle commodore mode
;
	mov	a,b		; get buffered matrix position to A
	cpi	alpha$toggle
	jrz	do$alpha$toggle
;
;	if normal mode(00), or in commodore mode bit
;
	inr	l		; point to control byte
	lda	commodore$mode
	ani	00100000b	; save commodore key set bit
	ora	m		; get rest of control byte
	mov	c,a
	ani	3
	mov	a,c
	jrnz	is$control$or$shift
	lda	commodore$mode
	ora	c

is$control$or$shift:
	dcr	l
	mvi	m,0ffh		; mark buffer position free	

	mov	l,b		; save matrix position in HL
	mvi	h,0
	dad	h
	dad	h		; mult. matrix position by 4
	mov	c,a		; save the control code in C for caller
	ani	3
	add	l		; add the offset 
	mov	l,a		; update the pointer
	lded	key$tbl		; get the start of the ASCII table
	dad	d		; HL now points to the ASCII value
	mov	a,m		; for the input key.
	ora	a		; set zero flag if A=0
	ret

test$caps:
	mvi	a,vic$PT$rd        
	call	?fun65
	lda	vic$data
	ani	40h
caps$st equ	$+1
	cpi	40h
	jrnz	is$caps
	lxi	b,-1
	xra	a
	ret
is$caps:
	sta	caps$st
	mvi	a,vic$80$init
	call	?fun65
	call	R$init$charset
	lda	caps$st
	ora	a
	jrnz	end$caps
	lxi	d,g$table
	mvi	b,6
	lxi	h,35b0h
	push	h
	RCALL	FR$char$inst
	mvi	b,4
	lxi	h,37b0h
	push	h
	RCALL	FR$char$inst
end$caps:
	lhld	gkey$tbl
	lxi	d,@buffer
	lxi	b,100h
	ldir
	lhld	key$tbl
	lded	gkey$tbl
	mvi	b,1		; lxi b,100h
	ldir
	lxi	h,@buffer
	lded	key$tbl
	mvi	b,1		; lxi b,100h
	ldir
	mvi	b,-1
	xra	a
	ret

g$table:
	db	5ah,24h,42h,7eh,42h,42h,42h,00h ; A umlaut
	db	5ah,24h,42h,42h,42h,24h,18h,00h ; O umlaut
	db	18h,42h,42h,42h,42h,42h,3ch,00h ; U umlaut
	db	10h,28h,44h,00h,00h,00h,00h,00h ; circumflex
	db	00h,00h,00h,00h,00h,00h,00h,0ffh; underline
	db	20h,10h,08h,00h,00h,00h,00h,00h ; backquote
	db	24h,00h,38h,04h,3ch,44h,3ah,00h ; a umlaut
	db	24h,00h,3ch,42h,42h,42h,3ch,00h ; o umlaut
	db	24h,00h,42h,42h,42h,46h,3ah,00h ; u umlaut
	db	3ch,42h,42h,5ch,42h,42h,5ch,40h ; sz ligature

	page

;
;	used to convert a keyboard matrix position into it's HEX
;	value (keys caps labelled with 0 to 9 and A to F)
;
hex$key$tbl:
	db	15h		; F
	db	0eh		; E
	db	12h		; D
	db	14h		; C
	db	1ch		; B
	db	0ah		; A
	db	20h		; 9
	db	1bh		; 8
	db	18h		; 7
	db	13h		; 6
	db	10h		; 5
	db	0bh		; 4
	db	08h		; 3
	db	3bh		; 2
	db	38h		; 1
	db	23h		; 0

