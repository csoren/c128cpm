
	title	'CXPRINTER    Commodore printer drivers    4 Dec 85'

	maclib	z80

	maclib	cxequ

	public	?PT$I$1101,?PT$O$1,?pt$o$2
	public	?convt
;	public	?PT$S$1101

	extrn	?fun65

;
;	printer output in register C
;
	dseg
?pt$o$2:
	lhld	prt$conv$2
	call	do$convt		; C must be unchanged A=desired code
	lxi	h,prt$buf$2
	mvi	b,5
	jr	prt$cont

do$convt:
	mov	a,c			; A=desired code
	mvi	c,7			; C=secondary address
	pchl				; HL,DE and B may be used

?pt$o$1:
	lhld	prt$conv$1
	call	do$convt
	lxi	h,prt$buf$1
	mvi	b,4
;
;
;
prt$cont:
	inr	m
	mov	e,m
	mvi	d,0
	xchg
	dad	d		; index into buffer
	mov	m,a
	xchg
;	ani	7Fh		; strip MSB
	cpi	lf		; data a CR ?
	jrz	print$it	; yes, go print this line
	mov	a,m		; no, get current line length
	cpi	prt$buf$lng-1	; reach end yet ?
	rnz			; no, exit
				; yes, print line of data
print$it:
	mov	a,m
	sta	vic$count	; set number of bytes to send
	mvi	m,0		; set count back to zero
	inx	h
	shld	@buffer		; save location to print from

	mov	a,b
	sta	vic$drv		; pass device # in Vic$drv
	mov	a,c
	sta	vic$trk		; pass secondary adr in Vic$trk

	mvi	a,vic$prt
	jmp	?fun65
;
;
;
?convt$none:
	mvi	c,0		; set secondary adr to 0
	ret
;
;
;
?convt:
	ani	7fh		; only allow real ASCII values for now
	cpi	cr
	jrz	set$msb
	cpi	'"'
	jrz	is$quote
	cpi	'@'
	rc

	cpi	60h
	jrc	make$upper$case
;
; if it is a lower case letter subtract 20h
;
	cpi	'z'+1
	jrnc	lower$symbols

	sui	20h
	ret

lower$symbols:
	adi	60h
	ret
;
;
make$upper$case:
	cpi	'Z'+1
	jrnc	upper$symbols
set$msb:
	adi	80h
	ret

;
;
is$quote:
	mvi	a,27h		; convert to tick (shifted 7)
	ret

;
;
upper$symbols:
	cpi	'\'
	rnz			; 	
	mvi	a,0ffh
;
;	printer initialization code
;
?pt$i$1101:
	ret


;
;	printer status code
;
	dseg
;?pt$s$1101:
;	ret


prt$buf$lng	equ	81

prt$buf$1:	ds	prt$buf$lng
prt$buf$2:	ds	prt$buf$lng

