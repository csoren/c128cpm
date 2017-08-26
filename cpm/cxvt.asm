


        title   'Terminal Emulation (VT-100)   18 Feb 86'

	maclib	cxequ

	if  use$VT100

	maclib	z80

lines	equ	24

	public	VT100

;
;	VT-100
;
;	NUL		00h	ignored
;	ENQ		05h	transmit answer back message
;	BEL		07h	ring bell
;	BS		08h	back space. stop at left margin
;	HT		09h	do TAB
;	LF		0Ah	do line feed scroll at bottom
;	VT		0Bh	same as LF
;	FF		0Ch	same as LF
;	CR		0Dh	do CR
;	SO		0Eh	invoke G1 set
;	SI		0Fh	invoke G0 set
;	XON		11h	ignored
;	XOFF		13h	ignored
;	CAN		18h	abort ESC seq (disp error character)
;	SUB		1Ah	same as CAN
;	ESC		1Bh	control seq
;	DEL		7Fh	not used
; 
;
;	ESC =			Keypad mode
;       ESC >                   Keypad mode
;	ESC 7			Save current cursor post and char set
;	ESC 8			Restore cursor position and char set
;	ESC D			move cursor down one line
;	ESC E			move cursor to start of next line
;	ESC H			set horizontal tab
;	ESC M			move cursor up one line
;	ESC Z			same as ESC [ Pn c
;	ESC c			reset
;	ESC # 3			Double height line Top
;	ESC # 4			Double height line Bottom
;	ESC # 5			set single width line
;	ESC # 6			Double width line
;	ESC # 8			files screen with E's
;	ESC [ Pn A		cursor up
;	ESC [ Pn B		cursor down
;	ESC [ Pn C		cursor right
;	ESC [ Pn D		cursor left
;	ESC [ Pn ; Pn H		cursor positioning
;	ESC [ Ps J		erase display
;	ESC [ Ps K		erase line
;	ESC [ Pn c		device attributes request
;	ESC [ Pn ; Pn f		cursor positioning
;	ESC [ Ps g		clear tab stop(s)
;	ESC [ Ps;..;Ps h	set mode
;	ESC [ Ps;..;Ps l	reset attributes
;	ESC [ Ps;..;Ps m	set attributes
;	ESC [ Ps n		Device status report
;	ESC [ Ps q		set LED's
;	ESC [ Pn ; Pn r		Set Top and Bottom Margins
;	ESC [ 2 ; Ps y		invoke confidence test
;	ESC [ x			Report / Req parameters
;;	ESC ( A			select char set
;;	ESC ( B			select char set
;;	ESC ( 0			select char set
;;	ESC ( 1			select char set
;;	ESC ( 2			select char set
;;	ESC ) A			select char set
;;	ESC ) B			select char set
;;	ESC ) 0			select char set
;;	ESC ) 1			select char set
;;	ESC ) 2			select char set
;
	page

	dseg
;
;	VT-100 terminal emulation
;
VT100:
	lhld	parm$base		; 1st parm is exec adr (2 bytes)
	mov	a,m
	inx	h
	mov	h,m
	mov	l,a
	ora	h			; L is in A already, test HL=0
	mov	a,c			; C is char to output
	jrz	start$checking 
	pchl
;
;
;
start$checking:
	lxi	h,control$table
	lxi	b,cnt$tbl$lng
	ccir
	lxi	h,control$exec$adr
	jrz	find$exec$adr
	cpi	20h
	rc

do$direct:
	mov	d,a
	TJMP	FR$wr$char

	page
;
;
;
do$ESC:		; ESC	control seq
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
;	ESC #
;
esc$pn:		; ESC # control seq
	call	cont$later
;
;	ESC # char	look for char in the ESC table
;
	call	remove$exec$adr
	lxi	h,esc$pnd$table
	lxi	b,esc$pnd$tbl$lng
	ccir
	rnz				; bad esc sequence
	lxi	h,esc$pnd$exec$adr
	jr	find$exec$adr

	page
;
;	ESC [
;
esc$br:		; ESC [
	call	clear$parm
	call	cont$later
;
;	ESC [ char	look for char in the ESC table
;
	cpi	'9'+1			; input char a parameter
	jrc	put$buffer		; yes, save parameters in buffer
	call	remove$exec$adr
	lxi	h,esc$br$table
	lxi	b,esc$br$tbl$lng
	ccir
	rnz				; bad esc sequence
	lxi	h,esc$br$exec$adr
	jr	find$exec$adr
;
;	put byte in buffer pointed to by the put pointer+1 (advance pointer)
;
put$buffer:
	mov	c,a			; save character in C
	call	get$par			; get address of parameter buffer
	mov	e,m			; get low byte adr of input buffer
	inx	h
	mov	d,m			; get high byte adr of input buffer
	inx	h
	inr	m			; advance input count
	mov	l,m			; get current count (with input)
	mvi	h,0			;
	dad	d			; compute adr in buf to place input
	mov	m,c			; place input character into buffer
;	stc
	ret
;
;	get byte from buffer pointed to by the get pointer+1 (advance pointer)
;
get$buffer:
	call	get$par
	mov	e,m
	inx	h
	mov	d,m
	inx	h
	mov	a,m		; recover put counter
	inx	h
	sub	m		; test for end
	rz
	inr	m		; advance get counter
	mov	l,m		; get the get counter
	mvi	h,0
	dad	d
	mov	a,m
do$DEL:		; DEL	not used
	ret

	page
;
;
;
do$CAN:		; CAN SUB  abort ESC seq (disp error character)

;
;	Invoke G0 char set
;
do$SI:		; SI	invoke G0 set
	ret

;
;	Invoke G1 char set
;
do$SO:		; SO	invoke G1 set
	ret

;
;	move cursor to margin on current line
;
do$CR:		; CR	do CR
	TJMP	FR$do$cr

;
;	
;
do$LF:		; FF VT LF	do line feed scroll at bottom
	TJMP	FR$cursor$down

;
;	move cursor to next tab stop or right margin if none
;
do$HT:		; HT	do TAB
	ret

;
;	move cursor to left but not past left margin
;
do$BS:		; BS	back space. stop at left margin
	TJMP	FR$cursor$left

;
;	Sound bell tone
;
do$BEL:		; BEL	ring bell
	RJMP	FR$bell

;
;	transmit answerback message
;
do$ENQ:		; ENQ	transmit answer back message
	ret

;
;
;
esc$pn$8:	; ESC # 8	files screen with E's
	lxi	d,024*256+0	; set row (D) and col (E)
	mvi	c,24		; set # of rows (C)
out$next$line$E:
	mvi	b,80		; set # of col (B)
	dcr	d		; start with row 0
	push	d
	push	b
	TCALL	FR$cursor$pos
	pop	b
out$next$E:
	push	b
	mvi	d,'E'
	TCALL	FR$wr$char
	pop	b
	djnz	out$next$E
	pop	d
	dcr	c
	jrnz	out$next$line$E
	ret

;
;
;
esc$pn$6:	; ESC # 6	Double width line
	ret

;
;
;
esc$pn$5:	; ESC # 5	set single width line
	ret

;
;
;
esc$pn$4:	; ESC # 4	Double height line Bottom
	ret

;
;
;
esc$pn$3:	; ESC # 3	Double height line Top
	ret

;
;	Set tab at current cursor column
;
esc$HH:		; ESC H	set horizontal tab
	ret

;
;	Move cursor down one line, scroll if on bottom margin
;
esc$DD:		; ESC D	move cursor down one line

	ret

;
;	Move cursor to start of next line, scroll up if cursor
;	is on the bottom margin
;
esc$EE:		; ESC E	move cursor to start of next line
	
	ret

;
;	Move cursor up one line, if on top margin scroll down
;
esc$MM:		; ESC M	move cursor up one line

	ret

;
;	reset VT100 to initial state (causes INIT H to be asserted
;	briefly ???)
;
esc$c:		; ESC c	reset
	ret

;
;
;
esc$8:		; ESC 8	Restore cursor position and char set
	ret

;
;
;
esc$7:		; ESC 7	Save current cursor post and char set
	ret

;
;	place Keypad into Numeric mode
;
esc$gt:         ; ESC > Keypad mode
	ret

;
;	place Keypad into Application mode
;
esc$equ:	; ESC =	Keypad mode
	ret

;
;
;
esc$br$y:	; ESC [2;Ps y	invoke confidence test
	ret

;
;
;
esc$br$x:	; ESC [ x	Report / Req parameters
	ret

;
;
;
esc$br$r:	; ESC [Pn;Pn r	Set Top and Bottom Margins
	ret

;
;	Ps=0	clear all LED's (default)
;	PS=1	set LED 1
;	Ps=2	set LED 2
;	Ps=3	set LED 3
;	Ps=4	set LED 4
;
esc$br$q:	; ESC [Ps q	set LED's
	ret

;
;	Ps=5	Status Report
;		responce is:	ESC [0n (terminal OK)
;				ESC [3n (terminal not OK)
;	Ps=6	Report cursor position
;		responce is:	ESC [ Pl ; Pc R
;		where Pl is the line number
;		and Pc is column number
;
esc$br$n:	; ESC [Ps n	Device status report
	ret

;
;	Ps=0	attributes off (default)
;	Ps=1	bold or increased intensity
;	Ps=4	underscore
;	Ps=5	blink
;	Ps=7	reverse video
;
esc$br$m:	; ESC [Ps;;Ps m set character attributes
	call	get$Pn$def0$init
check$br$m:
	dcr	c		; check # of parameters used
	rz			; exit if None
	ana	a		; Ps=0 ?
	jrz	set$atr$off	; set attributes off
	dcr	a		; Ps=1
	jrz	bold$on		;
	dcr	a		; Ps=2
	dcr	a		; Ps=3
	dcr	a		; Ps=4
	jrz	underline$on	;
	dcr	a		; Ps=5
	jrz	blink$on	;
	dcr	a		; Ps=6
	dcr	a		; Ps=7
	jrz	reverse$on	;
	call	get$Pn$def0
	jr	check$br$m

set$atr$off:
bold$on:
underline$on:
blink$on:
reverse$on:
	ret


;
;	Ps=1	cursor key	(l=cursor ; h=application)
;	Ps=2	ANSI/VT52	(l=VT52  not supported)
;	Ps=3	Column		(l=80 col ; h=132 col) 80 only
;	Ps=4	Scrolling	(l=jump ; h=smooth) smooth only
;	Ps=5	Screen		(l=normal ; h=reverse)
;	Ps=6	Origin		(l=Absolute ; h=Relative)
;	Ps=7	Auto wrap	(l=off ; h=on)
;	Ps=8	Auto Repeat	(l=off ; h=on)
;	Ps=9	interlace	(l=off ; h=on)
;	Ps=20	LF/NL		(l=line feed ; h=new line)
;
esc$br$l:	; ESC [Ps;;Ps l	reset mode
	ret
;
;	see esc$br$l 
;
esc$br$h:	; ESC [Ps;;Ps h	set mode
	ret

;
;	Ps=0	clear tab stop at current column (default)
;	Ps=3	clear ALL tab stops
;
esc$br$g:	; ESC [Ps g	clear tab stop(s)
	ret

;
;	Pn default =1	missing Pn uses default value(s)
;	position cursor to line (1st) and column (2nd)
;	uses DECOM parm to set origin mode (within margin
;	or full screen)
;
esc$br$f:	; ESC [Pn;Pn f	cursor positioning
esc$br$HH:	; ESC [Pn;Pn H	cursor positioning
	call	get$Pn$def1$init
	dcr	a
	mov	d,a
	call	get$Pn$def1		; DE are not changed by this call
	dcr	a
	mov	e,a
	TJMP	FR$cursor$pos

;
;	What are you
;	response is:	ESC [?1; Ps c
;	where Ps is:
;		0=base VT100, no options
;		1=processor option (STP)
;		2=advanced video option (AVO)
;		3=AVO and STP
;		4=graphics processor option (GPO)
;		5=GPO and STP
;		6=GPO and AVO
;		7=GPO, STP and AVO
;
esc$ZZ:		; ESC Z		same as ESC [ Pn c
esc$br$c:	; ESC [Pn c	device attributes request
	ret

;
;	Ps=0	from cursor to end of line (default)
;	Ps=1	from start of line to cursor
;	Ps=2	all of cursor line
;
esc$br$KK:	; ESC [Ps K	erase line
	ret

;
;	Ps=0	from cursor to end of screen
;	Ps=1	from start of screen to cursor
;	Ps=2	all of screen (cursor is not moved)
;
esc$br$JJ:	; ESC [Ps J	erase display
	ret

;
;
;
esc$br$DD:	; ESC [Pn D	cursor left
	ret

;
;
;
esc$br$CC:	; ESC [Pn C	cursor right
	ret

;
;
;
esc$br$BB:	; ESC [Pn B	cursor down
	ret

;
;
;
esc$br$AA:	; ESC [Pn A	cursor up
	ret

	page
;
;	convert number to binary
;	stop conversion at end or any none number
;	(DE may not be changed)
;
get$Pn$def0$init:
	call	init$get		; set up to read buffer
;
get$Pn$def0:
	call	get$in$parm
	mov	a,b			; get input data to A
	ret
;
;*****	NOTE	ESC [ ;4;A	is the same as ESC [ 0;4;5A
;
;
;	convert number to binary
;	stop conversion at end or any none number
;	return 1 if input is missing or a zero
;	(DE may not changed)
;
get$Pn$def1$init:
	call	init$get		; set up to read buffer
;
get$Pn$def1:
	call	get$in$parm
	mov	a,b			; get input data to A
	ora	a			; is input =0?
	rnz				; no, then use it
	inr	a			; yes, then use default of 1
	ret

;
;	B=converted number in binary (from input string)
;	C=number of digits converted+1
;	A=0 if ran out of input else A=last character read from string
;	(DE may not be changed)
;
get$in$parm:
	lxi	b,1			; B=0, C=1
get$next$num:
	lda	save$count
	dcr	a
	rz
	sta	save$count
	lhld	buff$pointer		; get input buffer adr
	inx	h			; PRE incr adr
	shld	buff$pointer
	mov	a,m
	call	test$num
	rc
	slar	b			; 2x
	add	b			; A=A+2B
	slar	b			; 4x
	slar	b			; 8x
	add	b			; A=A+2B+8B=A+10B
	mov	b,a			; save in B
	inr	c			; advance parmeter count
	jr	get$next$num

;
;	return with carry set (Cy=1) if not a number (A=input Char)
;	return bianary number in A if it was a number (Cy=0)
;
test$num:
	cpi	'0'
	rc
	cpi	'9'+1
	cmc
	rc
	sui	'0'
	ret
;
;	set up local values to use buffer parameters
;
init$get:
	call	get$par			; get pointer to buffer(s)
	mov	e,m
	inx	h
	mov	d,m
	inx	h
	mov	a,m
	inr	a			; adjust for PRE decr
	sta	save$count
;	inx	h
;	mov	a,m
;	sta	get$count
	xchg
	shld	buff$pointer
	ret

save$count:
	db	0
buff$pointer:
	dw	0

	page
;
;	set buffer back to start
;
clear$parm:
	call	get$par
	inx	h
	inx	h
	mvi	m,0		; zero out the input count
	ret
;
;
;
get$par:
	lxi	h,vt100$par$80
	lda	fun$offset
	ana	a
	rz
	lxi	h,vt100$par$40
	ret
;
;
;
vt100$par$80:
	dw	buffer$80
	db	0		; current put pointer into buffer
;
;
;
vt100$par$40:
	dw	buffer$40
	db	0		; current put pointer into buffer

buffer$80	equ	$-1
	ds	20
buffer$40	equ	$-1
	ds	20

	page
;
;
;
cont$later:
	pop	h		; get address to cont at in H
	jr	save$exec$adr	; save it
;
;
;
remove$exec$adr:
	lxi	h,0
save$exec$adr:
	xchg
	lhld	parm$base
	mov	m,e
	inx	h
	mov	m,d
	ret

	page
;
;	table scanned top to bottom
;
control$table:
	db	05h	; ENQ	transmit answer back message
	db	07h	; BEL	ring bell
	db	08h	; BS	back space. stop at left margin
	db	09h	; HT	do TAB
	db	0Ah	; LF	do line feed scroll at bottom
	db	0Bh	; VT	same as LF
	db	0Ch	; FF	same as LF
	db	0Dh	; CR	do CR
	db	0Eh	; SO	invoke G1 set
	db	0Fh	; SI	invoke G0 set
	db	18h	; CAN	abort ESC seq (disp error character)
	db	1Ah	; SUB	same as CAN
	db	1Bh	; ESC	control seq
	db	7Fh	; DEL	not used

cnt$tbl$lng	equ	$-control$table

;
;	table scanned bottom to top
;
control$exec$adr:
	dw	do$DEL		; DEL	not used
	dw	do$ESC		; ESC	control seq
	dw	do$CAN		; SUB	same as CAN
	dw	do$CAN		; CAN	abort ESC seq (disp error character)
	dw	do$SI		; SI	invoke G0 set
	dw	do$SO		; SO	invoke G1 set
	dw	do$CR		; CR	do CR
	dw	do$LF		; FF	same as LF
	dw	do$LF		; VT	same as LF
	dw	do$LF		; LF	do line feed scroll at bottom
	dw	do$HT		; HT	do TAB
	dw	do$BS		; BS	back space. stop at left margin
	dw	do$BEL		; BEL	ring bell
	dw	do$ENQ		; ENQ	transmit answer back message

	page
;
;	table scanned top to bottom
;
esc$table:
	db	'='		; ESC =	Keypad mode
        db      '>'             ; ESC > Keypad mode
	db	'7'		; ESC 7	Save current cursor post and char set
	db	'8'		; ESC 8	Restore cursor position and char set
	db	'D'		; ESC D	move cursor down one line
	db	'E'		; ESC E	move cursor to start of next line
	db	'H'		; ESC H	set horizontal tab
	db	'M'		; ESC M	move cursor up one line
	db	'Z'		; ESC Z	same as ESC [ Pn c
	db	'c'		; ESC c	reset
	db	'#'		; ESC # control seq
	db	'['		; ESC [ cursor up
esc$tbl$lng	equ	$-esc$table

;
;	table scanned bottom to top
;
esc$exec$adr:
	dw	esc$br		; ESC [ cursor up
	dw	esc$pn		; ESC # control seq
	dw	esc$c		; ESC c reset
	dw	esc$ZZ		; ESC Z	same as ESC [ Pn c
	dw	esc$MM		; ESC M move cursor up one line
	dw	esc$HH		; ESC H	set horizontal tab
	dw	esc$EE		; ESC E move cursor to start of next line
	dw	esc$DD		; ESC D move cursor down one line
	dw	esc$8		; ESC 8	Restore cursor position and char set
	dw	esc$7		; ESC 7	Save current cursor post and char set
        dw      esc$gt          ; ESC > Keypad mode
	dw	esc$equ		; ESC =	Keypad mode

;
;
;
esc$pnd$table:
	db	'3'	; ESC # 3	Double height line Top
	db	'4'	; ESC # 4	Double height line Bottom
	db	'5'	; ESC # 5	set single width line
	db	'6'	; ESC # 6	Double width line
	db	'8'	; ESC # 8	files screen with E's

esc$pnd$tbl$lng	equ	$-esc$pnd$table

esc$pnd$exec$adr:
	dw	esc$pn$8	; ESC # 8	files screen with E's
	dw	esc$pn$6	; ESC # 6	Double width line
	dw	esc$pn$5	; ESC # 5	set single width line
	dw	esc$pn$4	; ESC # 4	Double height line Bottom
	dw	esc$pn$3	; ESC # 3	Double height line Top

;
;
;
esc$br$table:
	db	'A'	; ESC [ Pn A		cursor up
	db	'B'	; ESC [ Pn B		cursor down
	db	'C'	; ESC [ Pn C		cursor right
	db	'D'	; ESC [ Pn D		cursor left
	db	'H'	; ESC [ Pn ; Pn H	cursor positioning
	db	'J'	; ESC [ Ps J		erase display
	db	'K'	; ESC [ Ps K		erase line
	db	'c'	; ESC [ Pn c		device attributes request
	db	'f'	; ESC [ Pn ; Pn f	cursor positioning
	db	'g'	; ESC [ Ps g		clear tab stop(s)
	db	'h'	; ESC [ Ps;..;Ps h	set mode
	db	'l'	; ESC [ Ps;..;Ps l	reset attributes
	db	'm'	; ESC [ Ps;..;Ps m	set attributes
	db	'n'	; ESC [ Ps n		Device status report
	db	'q'	; ESC [ Ps q		set LED's
	db	'r'	; ESC [ Pn ; Pn r	Set Top and Bottom Margins
	db	'x'	; ESC [ x		Report / Req parameters
	db	'y'	; ESC [ 2 ; Ps y	invoke confidence test

esc$br$tbl$lng	equ	$-esc$br$table

esc$br$exec$adr:
	dw	esc$br$y	; ESC [2;Ps y	invoke confidence test
	dw	esc$br$x	; ESC [ x	Report / Req parameters
	dw	esc$br$r	; ESC [Pn;Pn r	Set Top and Bottom Margins
	dw	esc$br$q	; ESC [Ps q	set LED's
	dw	esc$br$n	; ESC [Ps n	Device status report
	dw	esc$br$m	; ESC [Ps;;Ps m set attributes
	dw	esc$br$l	; ESC [Ps;;Ps l	reset attributes
	dw	esc$br$h	; ESC [Ps;;Ps h	set mode
	dw	esc$br$g	; ESC [Ps g	clear tab stop(s)
	dw	esc$br$f	; ESC [Pn;Pn f	cursor positioning
	dw	esc$br$c	; ESC [Pn c	device attributes request
	dw	esc$br$KK	; ESC [Ps K	erase line
	dw	esc$br$JJ	; ESC [Ps J	erase display
	dw	esc$br$HH	; ESC [Pn;Pn H	cursor positioning
	dw	esc$br$DD	; ESC [Pn D	cursor left
	dw	esc$br$CC	; ESC [Pn C	cursor right
	dw	esc$br$BB	; ESC [Pn B	cursor down
	dw	esc$br$AA	; ESC [Pn A	cursor up

	endif
	end

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
	jrnz	not$status$line		; no, jmp
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
cursor$rt:
	TJMP	FR$cursor$rt

;
;
;
cursor$up:
	TJMP	FR$cursor$up


	page

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
;
;	Half Intensity Off
;
esc$lfp:
	mvi	c,00000001b		; turn intensity up
	jr	set$FR$atr$c
;
;	Half Intensity On
;
esc$rtp:
	mvi	c,00000000b		; turn intensity down
parn$cont:
	mvi	b,00000001b		; attribute bit to change
	jr	set$FR$attr

;
;	Set Attribute sequence
;
esc$G:
	call	cont$later
;
;	ESC G char
;
	call	remove$exec$adr
	sui	'4'			; '4' reverse video on
	jrz	esc$G$4
	inr	a			; '3' underline attr on
	jrz	esc$G$3
	inr	a			; '2' blink attr on
	jrz	esc$G$2
	inr	a			; '1' alt char set
	jrz	esc$G$1
	inr	a			; '0' clear attributes
	rnz
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
	mvi	c,00000000b		; select alt character set
	mvi	b,10000000b
	jr	set$FR$attr

;
;	Blinking On
;
esc$G$2:	
	mvi	c,00010000b		; turn on blink attr
	jr	set$FR$atr$c

;
;	Under line
;
esc$G$3:
	mvi	c,00100000b		; turn on underline bit
	jr	set$FR$atr$c

;
;	Reverse Video On
;
esc$G$4:
	mvi	c,01000000b		; turn attributes on

set$FR$atr$c:
	mov	b,c			; reverse attr
set$FR$attr:
	TJMP	FR$attr

