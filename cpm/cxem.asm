

	title	'Terminal Emulation (ADM-31 with K-Pro support)   21 May 86'


	maclib	z80

	maclib	cxequ

lines	equ	24

	public	?out40,?out80,ADM31,start$checking

;
;	ADM3A
;
;
;	ESC = row col		cursor position
;	ESC ESC ESC color	set color		; added for C128 CP/M
;	^H			cursor left
;	^L			cursor right
;	^J			cursor down
;	^K			cursor up
;	^Z			home and clear screen
;	^M			carrage return
;	^G			bell

;
;	ADM31
;
;
;	ESC = row col	cursor position
;	ESC ESC ESC color	set color		; added for C128 CP/M
;	ESC T		clear to end of line
;	ESC t		clear to end of line
;	ESC Y		clear to end of screen
;	ESC y		clear to end of screen
;	ESC :		home & clear screen
;	ESC *		home & clear screen
;	ESC )		Half intensity on
;	ESC (		Half intensity off
;	ESC G 4		Reverse video on
;	ESC G 2		Blinking on
;	ESC G 0		Rev. video and blinking off
;	ESC E		Insert line
;	ESC Q		Insert Character
;	ESC R		Delete Line
;	ESC W		Delete Character
;	^H		cursor left
;	^L		cursor right
;	^J		cursor down
;	^K		cursor up
;	^Z		home and clear screen
;	^M		carriage return
;	^G		bell
;
	page
;
;	KPRO II Terminal control sequences
;
;
; Cursor Control
;
;	^H	cursor left (bs)
;	^L	cursor right
;	^J	cursor down
;	^K	cursor up
;	^^	home cursor
;	^Z	home cursor & clear screen
;	^M	carriage return
;
; Cursor Positioning
;
;	ESC = R C	(R & C =' '+position)
;
; Line Insert/Delete
;
;	ESC E	Line Insert
;	ESC R	Line Delete
;
; Clear to End of Screen/Line
;
;	^X	Clear to End of Line
;	^W	Clear to End of Screen
;
; Set Greek or ASCII (not supported)
;
;	ESC A	Set ASCII
;	ESC G	Set Greek	(lower case letters print as Greek Alphabet)
;
; KAYPRO 84 (???) screen commands
;
;	ESC B <num>	turn attribute on
;	ESC C <num>	turn attribute off
;
;	where <num> is defined as:
;		0=reverse video
;		1=	<half intensity>
;		2=	<blink>
;		3=	<underline>
;
;			<best guess>
;
;		The following two sequences are
;		 use but I do not know what function
;		 they perform.  (added 21 May 86)
;
;	ESC D <num1><num2><num3><num4>
;	ESC L <num1><num2><num3><num4>
;
	page

	dseg
;
;
;
?out40:
	mvi	a,FR$40
	sta	fun$offset
	lxi	h,parm$area$40
	shld	parm$base
	call	?out$80
	xra	a
	sta	fun$offset
	lxi	h,parm$area$80
	shld	parm$base
	ret

;
?out$80:
	mvi	a,7fh
	ana	c

ADM31:
	lhld	parm$base               ; 1st parm is exec adr (2 bytes)
	mov	c,m
	inr	l
	mov	h,m
	mov	l,c
	pchl

start$checking:
	cpi	20h
	jrc	contrl$char
	mov	d,a

	TJMP	FR$wr$char




;
;
;

contrl$char
	lxi	h,control$table
	lxi	b,cnt$tbl$lng
	ccir
	lxi	h,control$exec$adr
	jrz	find$exec$adr

	ret

	page
;
;
;
char$esc:				; ESC
	call	cont$later
;
;	ESC char	look for char in the ESC table
;
	call	remove$exec$adr
	lxi	h,esc$table
	lxi	b,esc$tbl$lng
	ccir
	rnz				; bad esc sequence
	lxi	h,esc$exec$adr

find$exec$adr:
	dad	b
	dad	b
	mov	a,m
	inx	h
	mov	h,m
	mov	l,a
	pchl



	page
;
;
;
cont$later:
	pop	h		; get address to cont at in H
;
;
;
save$exec$adr:
	xchg
	lhld	parm$base
	mov	m,e
	inx	h
	mov	m,d
	ret

remove$exec$adr:
	lxi	h,start$checking
	jr	save$exec$adr

;
;
;
esc$esc:
	call	cont$later
;
;	check for ESC ESC ESC
;
	cpi	esc			; check if 3rd char is an ESC
	jrnz	remove$exec$adr
	call	cont$later
;
;	set current character as the attr
;
	mov	b,a
	TCALL	FR$color
	jr	remove$exec$adr

	page
;
;
;
esc$equ:
	call	cont$later
;
;	ESC = R
;
	lhld	parm$base
	inx	h
	inx	h
	sui	' '			; remove ascii bias
	mov	m,a
	cpi	'8'-' '			; test for line 25 (A=24?)
	jrnz	not$status$line         ; no, jmp
	inr	a			; yes, A=25
	sta	paint$size		; set 40 column repaint to 25 lines
not$status$line:
	call	cont$later
;
;	ESC = R C	(go do it)
;
	sui	' '
	mov	e,a			; column # to E

	lhld	parm$base
	inx	h
	inx	h
	mov	d,m			; row # to D
	TCALL	FR$cursor$pos
	jr	remove$exec$adr

	page

;
;
;
	nop
char$cnt$z:				; ^Z	home and clear screen
	lxi	d,lines*256+0		; B=24(row) C=0(col)
	TCALL	FR$cursor$pos
	call	esc$t			; clear the status line 
	lxi	d,0
	TCALL	FR$cursor$pos
esc$y:
	TJMP	FR$CES			; clear to end of screen 

home$cursor:
	lxi	d,0
	TJMP	FR$cursor$pos
	
esc$t:
	TJMP	FR$CEL			; clear to end of line 

;
;
;
do$cr:
	TJMP	FR$do$cr

;
;
;
cursor$rt:
	TJMP	FR$cursor$rt

;
;
;
cursor$up:
	TJMP	FR$cursor$up

;
;
;
cursor$down:
	TJMP	FR$cursor$down

;
;
;
cursor$left:
	TJMP	FR$cursor$left

	page

;
;	placed in common so that link and gencpm will not
;	cause this code to show up at address 0D000h to 0DFFFh
;

char$cnt$g:				; ^G	bell
	RJMP	FR$bell

;
;	delete character
;
esc$W:
	TJMP	FR$char$del

;
;	delete line
;
esc$R:
	TJMP	FR$line$del

;
;	insert character
;
esc$Q:
	TJMP	FR$char$ins

;
;	insert line
;
esc$E:
	TJMP	FR$line$ins

	page

	dseg
;
;	ESC C <num> atribute off
;
esc$C:
	call	cont$later
	lxi	b,4*256+4		; max+1 num, offset
	jr	esc$num$cont

;
;	ESC B <num> atribute on
;
esc$B:
	call	cont$later
	lxi	b,4*256+0		; max+1 num, offset
	jr	esc$num$cont
;
;	Set Attribute sequence
;
esc$G:
	call	cont$later
	lxi	b,5*256+8		; max+1 num, table offset
esc$num$cont:
	call	remove$exec$adr
	sui	'0'			; remove ascii bias
	cmp	b			; number of functions
	rnc
	add	c			; get offset
	mov	c,a
	mvi	b,0
	lxi	h,esc$num$tbl
	jmp	find$exec$adr

	page
;
;
;
esc$D:
esc$L:
	call	cont$later		; wait for num1
	call	cont$later		; wait for num2
	call	cont$later		; wait for num3
	call	cont$later		; wait for num4 
	jmp	remove$exec$adr

	page
;
;	Half Intensity Off
;
esc$lfp:
	mvi	b,00000001b		; turn intensity up
	jr	set$atr$on
;
;	Half Intensity On
;
esc$rtp:
	mvi	b,00000001b		; turn intensity down
	jr	set$atr$off

;
;	Rev. Video, blink, atl char set, and underline  off
;
esc$G$0:
	mvi	c,10000000b		; turn attributes off
	mvi	b,11110000b		; attribute bit to change
	jr	set$FR$attr

;
;	Select alt character set
;
esc$G$1:
	mvi	b,10000000b		; select alt character set
	jr	set$atr$off

;
;	Blinking On
;
esc$B$2:		; turn flash ON ???
esc$G$2:	
	mvi	b,00010000b		; turn on blink attr
	jr	set$atr$on
;
;
;
esc$C$2:		; turn flash OFF ???
	mvi	b,00010000b
	jr	set$atr$off
;
;	Under line
;
esc$B$3:		; turn underline ON ???
esc$G$3:
	mvi	b,00100000b		; turn on underline bit
	jr	set$atr$on
;
;
;
esc$C$3:		; turn under line OFF  ???
	mvi	b,00100000b
	jr	set$atr$off
;
;	Reverse Video On
;
esc$B$0:
esc$G$4:
	mvi	b,01000000b		; turn attributes on

set$atr$on:
	mov	c,b			; reverse attr
	jr	set$FR$attr

set$FR$attr:
	TJMP	FR$attr
	dseg
;
;
;
esc$C$1:		; turn half bright OFF ???
	mvi	b,00000001b
	jr	set$atr$on

;
;
;
esc$B$1:		; set half bright ON ???
	mvi	b,00000001b
	jr	set$atr$off

;
;	turn reverse video off
;
esc$C$0:
	mvi	b,01000000b		; attribute to turn off
set$atr$off:
	mov	a,b
	cma
	ana	b
	mov	c,a
	jr	set$FR$attr

	page
;
;	table scanned top to bottom
;
control$table:
	db	esc	; ESC
	db	cr	; ^M		carrage return
	db	lf	; ^J		cursor down


	db	bs	; ^H		cursor left

	db	0Bh	; ^K		cursor up
	db	0Ch	; ^L		cursor right
	db	07h	; ^G		bell
	db	1Ah	; ^Z		home and clear screen
	db	18h	; ^X		Clear to End of Line (K-Pro)
	db	17h	; ^W		Clear to End of Screen (K-Pro)
	db	1Eh	; ^^		home cursor (K-Pro)

cnt$tbl$lng	equ	$-control$table

;
;	table scanned bottom to top
;
control$exec$adr:
	dw	home$cursor	; ^^	home cursor	(K-Pro)
	dw	esc$y		; ^W	CES		(K-Pro)
	dw	esc$t		; ^X	CEL		(K-Pro)
	dw	char$cnt$z	; ^Z	home and clear screen
	dw	char$cnt$g	; ^G	bell
	dw	cursor$rt	; ^L	cursor right
	dw	cursor$up	; ^K	cursor up
	dw	cursor$left	; ^H	cursor left
	dw	cursor$down	; ^J	cursor down
	dw	do$cr		; ^M	carriage return
	dw	char$esc	; ESC

	page
;
;	table scanned top to bottom
;
esc$table:
	db	'='		; ESC = R C	

	db	'T'		; ESC T	  clear to end of line
	db	't'		; ESC t   clear to end of line

	db	'E'		; ESC E   Insert line
	db	'Q'		; ESC Q   Insert Character
	db	'R'		; ESC R   Delete Line
	db	'W'		; ESC W   Delete Character

	db	'Y'		; ESC Y   clear to end of screen
	db	'y'		; ESC y   clear to end of screen
	db	':'		; ESC :   home & clear screen
	db	'*'		; ESC *   home & clear screen


	db	')'		; ESC )   Half intensity on
	db	'('		; ESC (   Half intensity off
	db	'G'		; ESC G 4 Reverse video on
				; ESC G 2 Blinking on
				; ESC G 0 Rev. video and blinking off
	db	'B'		; ESC B <num> atribute on
	db	'C'		; ESC C <num> atribute off
	db	esc		; ESC ESC
	db	'D'		; ESC D   ???
	db	'L'		; ESC L   ???

esc$tbl$lng	equ	$-esc$table


;
;	table scanned bottom to top
;
esc$exec$adr:
	dw	esc$L		; ESC L   A kaypro function ???
	dw	esc$D		; ESC D   A kaypro function ???
	dw	esc$esc		; ESC ESC ESC color
	dw	esc$C		; ESC C <num> atribute off
	dw	esc$B		; ESC B <num> atribute on
	dw	esc$G		; ESC G 4 Reverse video on
				; ESC G 2 Blinking on
				; ESC G 0 Rev. video and blinking off
	dw	esc$lfp		; ESC (   Half intensity off
	dw	esc$rtp		; ESC )   Half intensity on


	dw	char$cnt$z	; ESC *   home & clear screen
	dw	char$cnt$z	; ESC :   home & clear screen
	dw	esc$y		; ESC y   clear to end of screen
	dw	esc$y		; ESC Y   clear to end of screen

	dw	esc$W		; ESC W   Delete Character
	dw	esc$R		; ESC R   Delete Line
	dw	esc$Q		; ESC Q   Insert Character
	dw	esc$E		; ESC E   Insert line

	dw	esc$t		; ESC t   clear to end of line
	dw	esc$t		; ESC T	  clear to end of line

	dw	esc$equ		; ESC = RC
;
;
;
esc$num$tbl:
	dw	esc$b$0		; ESC B0	reverse video ON
	dw	esc$b$1		; ESC B1 ???	half bright ON
	dw	esc$b$2		; ESC B2 ???	blink ON
	dw	esc$b$3		; ESC B3 ???	under line ON

	dw	esc$c$0		; ESC C0	reverse video OFF
	dw	esc$c$1		; ESC C1 ???	half bright OFF
	dw	esc$c$2		; ESC C2 ???	blink OFF
	dw	esc$c$3		; ESC C3 ???	under line OFF

	dw	esc$g$0		; ESC G0 clear attributes (all G functions)
	dw	esc$g$1		; ESC G1 alt char set
	dw	esc$g$2		; ESC G2 blink attr on
	dw	esc$g$3		; ESC G3 underline attr on
	dw	esc$g$4		; ESC G4 reverse video on

