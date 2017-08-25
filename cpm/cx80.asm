

	title	'CX40 & CX80   40 and 80 column drivers    18 Feb 86'

	maclib	cxequ

	maclib	z80


lines	equ	24

;	public	?fundir			; function direct

	public	?int40,?int80
	public	?stat,?save,?recov,@st40

	extrn	ADM31,setadm
;	extrn	setvt

	page
;
;**
;**	This is the entry point to get to the function module
;** 
;
;
;	This code will perform the functions that the emulation
;	code will need to do to complete function.
;
;
;
;
;	enable  cursor,  then set foreground and background colors  
;
	DSEG
?int80:
	call	setadm
	lhld	key$tbl			; logical color assignments at end of
	lxi	d,11*4*8		; ..key$table, (key$tbl size=11*4*8)
	dad	d
	shld	color$tbl$ptr		; setup color table ptr

	mvi	a,80h
	sta	current$atr

;
;	program the 8563 for full flashing cursor
;
	mvi	a,10			; point to cursor start line#
	call	R$wait			;  and mode register
	mvi	a,40h			; start at line zero, cursor 1/16
	outp	a
	mvi	a,11			; point to cursor end line#
	call	R$wait
	mvi	a,7
	outp	a
	ret

	page
;
;
;
	DSEG
?int40:
	lxi	h,screen$40
	shld	char$adr$40

	mvi	a,24
	sta	paint$size		; set 40 column repaint to 24 lines

	lxi	b,VIC+18h		; point to address register
	mvi	a,vic$screen*4/256+6	; upper and lower case set (+6)
	outp	a			; move screen
	ret


;**
;**	The following code is used to maintain the status line on
;**	both the 80 and 40 column displays
;**
;
;	save characters on the status line (80 column only) to buffer
;	reverse video the data area cleared (40 and 80 column screens)
;
;	C=start column #	B=number of characters to save
;
	DSEG
?save:
	mov	a,c
	cpi	40
	jrnc	do$save$80

	push	b
;;;***
	mov	a,b
	lxi	h,stat$line$40
	mvi	b,0
	dad	b

clear$loop$40:
	mvi	m,' '+80h		; set reverse video
	inx	h
	dcr	a
	jrnz	clear$loop$40
	call	paint$40$status
;;;***
	pop	b

do$save$80:
	mov	a,b
	lxi	h,lines*80		; point to status line
	lxi	d,buffer$80$col		; point to save buffer
	mvi	b,0			; zero MSB
	dad	b			; point to char position to save

save$loop:
	push	psw			; save count
	push	d			; save buffer address
	call	R$read$memory		; read char(B) and attribute(A)
	pop	d			; recover buffer pointer
	stax	d			; save character
	inx	d			; advance buffer
	mov	a,b			; get atrb to A
	stax	d			; save atrb
	inx	d			; advance buffer
	push	d

	mvi	a,01000000b		; reverse video only
	call	get$atr$color		; returned in A
	mvi	d,' '			; get character
	call	R$write$memory

	pop	d
	pop	psw			; recover count
	inx	h
	dcr	a			; adjust count
	jrnz	save$loop		; loop if not done
	ret

	page
;
;	recover characters to the status line (80 column only)
;	for the 40 column screen just clear status line (with spaces)
;
;	C=start column #	B=number of characters to restore
;
	DSEG
?recov:
	mov	a,c
	cpi	40
	jrnc	recove$80		; skip 40 column if C>40

	push	b
;;;***
	lxi	h,stat$line$40
	mov	a,b
	mvi	b,0
	dad	b
	mov	b,a

recov$40$loop:
	mvi	m,' '
	inx	h
	djnz	recov$40$loop
	call	paint$40$status 
;;;***
	pop	b

recove$80:
	mov	a,b
	lxi	h,lines*80		; point to status line
	lxi	d,buffer$80$col		; point to save buffer
	mvi	b,0			; zero MSB
	dad	b			; point to char position to save

recov$80$loop:
	push	psw			; save count
	ldax	d			; get attribute
	inx	d			; advance pointer
	mov	b,a			; save attribute in B
	ldax	d			; get character in A
	inx	d			; advance pointer
	push	d			; save buffer address
	mov	d,a			; move character to D
	mov	a,b			; move attribute to A
	call	R$write$memory		; write char(D) and attribute(A)
	pop	d			; recover buffer pointer
	pop	psw			; recover count
	inx	h
	dcr	a			; adjust count
	jrnz	recov$80$loop		; loop if not done
	ret

	page
;
;	Places data on the system status line
;
;	for the 80 column screen a number of character attributes
;	are available: flash, underline, reverse video
;
;	for the 40 column screen only reverse video is available
;
;	INPUT:
;		A=attribute  (7654 3210)
;			6-reverse video
;			5-underline
;			4-blink
;		B=character to write (ASCII)
;		C=column number to write
;			(>40 does nothing to 40 column screen)
;
	DSEG
?stat:
	push	psw
	push	b			; save for 80 column display
	mov	e,a			; save attribute in E
	mov	a,c
	cpi	40
	jrnc	not$40$col$wr

;;;***
;
;	display on 40 column display 1st
;
	RCALL	FR$ASCII$to$pet		; char to convert is in B
					; returned in A
	mov	b,a
	mov	a,e			; get attribute byte
	ani	01000000b		; check for reverse video
	mov	a,b			; get pet ascii character
	jrz	char$not$rev
	ori	80h			; set MSB for reverse video
char$not$rev:
	mvi	b,0
	lxi	h,stat$line$40
	dad	b			; point to status position
	mov	m,a	
	call	paint$40$status
;;;***

;
;	display on 80 column display now
;
not$40$col$wr:
	pop	d			; D=character  E=position
	pop	psw			; get new attribute
	call	get$atr$color

	mov	b,d			; save character to write in B
	lxi	h,lines*80
	mvi	d,0
	dad	d			; point to character location
	mov	d,b			; place character to write in D
	jmp	R$write$memory


;
;	using attribute in A add color to it and return in A
;
;	destroys BC
;
	DSEG
get$atr$color:
	push	h
	push	psw
	lda	bg$color$80
	mov	c,a
	mvi	b,0
	lxi	h,status$color$tbl
	dad	b			; point to status color 
	pop	psw
	ani	01110000b		; limit good attr
	mov	b,a			; save in E
	mov	a,m			; get status color
	ani	0fh			; only want 80 column status color
	ora	b			; merge with new attr
	ori	80h			; select alternate character set
	pop	h
	ret

;
;
;
	DSEG
paint$40$status:
	lxi	h,stat$line$40
	lxi	d,vic$screen+24*40
	lxi	b,40
	ldir

	lda	bg$color$40
	mov	c,a
	mvi	b,0
	lxi	h,status$color$tbl
	dad	b
	mov	a,m
	rrc
	rrc				; move status color to LSB
	rrc				; no need to mask it
	rrc				; color RAM only 4 bits wide

	lxi	h,vic$color+24*40
	lxi	d,vic$color+24*40+1
	lxi	b,40
	jmp	paint$common

;
;
;
	CSEG
paint$common:
	sta	io$0
	mov	m,a
	ldir
	sta	bank$0
	ret

	page
;
;
;
@st40:
stat$line$40:
;		 12345678901234567890   character locations
	db	'                    '
	db	'                    '

;
;	MSB is 40 column status color, LSB is 80 column status color
;
status$color$tbl:
	db	05eh			; status color #1
	db	0f6h			; status color #2
	db	0a6h			; status color #3
	db	0b7h			; status color #4
	db	0d7h			; status color #5
 	db	0d4h			; status color #6
	db	0e7h			; status color #7
	db	083h			; status color #8
	db	097h			; status color #9
	db	0a8h			; status color #10
	db	09eh			; status color #11
	db	0ffh			; status color #12
	db	0bdh			; status color #13
	db	058h			; status color #14
	db	06fh			; status color #15
	db	0ceh			; status color #16


