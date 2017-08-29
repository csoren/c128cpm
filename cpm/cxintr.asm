


	title	'Interrupt handler    29 Apr 86'


	maclib	z80

	maclib	cxequ

	public	?sysint


done$scan:	equ	11110111b

clear$TxD$bit:	equ	10010111b	; 2nd byte of   res  2,a
set$TxD$bit:	equ	11010111b	; 2nd byte of   setb 2,a

buf$end	equ	low(RxD$buffer+RxD$buf$size)

	page
;
;	The DE register is not changed by the interrupt handler
;
;		maximun of     T states advaliable per interrupt
;		DMA uses about 10 % (or   ) leaving only 
;		interrupt vectoring use a few more.
;
;		if both recv$state and send$state are in idle
;	T states   209+82++ (191max,38min) = (482max,329min)
;
;
;		if ether recv$state and send$state are active
;	T states   209+++ (289max, 82min) = (498max,291min)
;
	CSEG
?sysint:
	sspd	ld$sp+1                 ;20
	lxi	sp,intr$stack           ;10
	push	psw			;11
	push	b			;11
	push	h			;11
;
	lxi	b,CIA$1+int$ctrl	;10
	inp	a			;12  clear CIA$1 interrupts
;
   if	not use$6551
	lxi	b,CIA2+data$a		;10
	inp	a			;12
out$rs232$cia	equ	$+1
	setb	2,a			;8   this instruction gets modified
	outp	a			;12

	inr	c			;4   point to data$b (C=1)
	inp	a			;12
	mov	h,c			;4   set H=1
recv$state:
	call	recv$idle		;17+(153max,54min)

send$state:
	call	send$idle		;17+(136max,28min)

	dcr	h			;4     did H=1 ?
	lxi	h,current$key$delay	;10
	jnz	skip$keyboard		;10

	page
;
;	T states  32	if not done
;	T states  56+	if key scan done
;
vector$key$state:
	dcr	m			;11
	jrnz	exit$int		;7/12
	lda	int$rate		;13
	mov	m,a			;7
   endif
key$state:
	call	key$scan$state		;17+(191max,38min)
   if	not use$6551
	db	21h			; lxi h,(mvi m,1)
skip$keyboard:
	mvi	m,1			;
   endif
exit$int:
	pop	h
	pop	b
	pop	psw
ld$sp:
	lxi	sp,0			; modified above
	ei
	ret

   if	not use$6551
RxD$count:
	db	0		; number of bits left to receive
TxD$count:
	db	0		; number of bits left to transmit

current$key$delay:
	db	1

	page
;
;	T states  52	start bit
;	T states  	no start bit, que inactive 
;	T states  	no start bit, que active, DAV set
;	T states 	no start bit, que active, DAV cleared
;
recv$idle:
	rar				;4
	jrnc	set$test$start$bit	;7/(12+36)

;11+12+31=54
;
RxD$unque:			;(36)+12+(105) = 153 max
	jr	test$que		;12

;
;	T states  31	no process required
;	T states  91	que count adjusted (not empty)
;	t states 105	que count adjusted (empty)
;
test$que:
	lda	RS232$status		;13	no processing req if QUE
	ani	00100000b		;7	bit (5) is clear
	rz				;5/11

	mvi	a,que$to$data-test$que	;7
	sta	RxD$unque+1		;13	set next sub state

	lxi	h,RxD$buf$get		;10
	inr	m			;11
	mov	a,m			;7
	cpi	buf$end			;7
	rnz				;5/11
	mvi	m,low(RxD$buffer)	;10
	ret				;10

;
set$test$start$bit:
	lxi	h,test$start$bit	;10
	shld	recv$state+1		;16
	ret				;10

	page
;
;	T states  28	if DAV still set
;	T states  89	to move char from que to recv$data
;
que$to$data:
	lda	RS232$status		;13
	rrc				;4
	rc				;5/11

	lxi	h,RxD$buf$get		;10
	mov	l,m			;7
	mov	a,m			;7
	sta	recv$data		;13

	mvi	a,adjust$cnt-test$que	;7
	sta	RxD$unque+1		;13	set next sub state
	ret				;10

;
;	T states  82	count not zero
;	T states  99	count becomes zero 
;
adjust$cnt:
	xra	a			;4
	lxi	h,RxD$buf$count		;10
	dcr	m			;11
	mvi	l,low(RS232$status)	;7
	setb	0,m			;15	set DAV flag
	jrnz	adj$cont		;7/12
	res	5,m			;15	que empty turn QUE bit(5) off
	mvi	a,que$empty-test$que	;7
adj$cont:
	sta	RxD$unque+1		;13
	ret				;10
;
;	T states  52/94
;
que$empty:
	xra	a			;4	offset of zero for JR
	sta	RxD$unque+1		;13	(to get to test$que)
	lxi	h,xon$xoff$flag		;10
	mvi	a,XON			;7
	cmp	m			;7
	rz				;5/11
	mov	m,a			;7
	mvi	a,send$x-send$norm	;7
	sta	send$idle+1		;13
	ret				;10


	page
;
;	test for false start
;
;	T states  72	if valid start
;	T states  52	if false start
;
test$start$bit:
	rar				;4
	jrc	set$recv$idle		;7/(12+36)	RxD in carry bit
	lxi	h,RS232$status		;10
	setb	1,m			;15	set receiving data flag
	lxi	h,start$idle$1		;10
	shld	recv$state+1		;16
	ret				;10

;
;	T states  36
;
set$recv$idle:
	lxi	h,recv$idle		;10
	shld	recv$state+1		;16
	ret				;10


;
;	T states  93
;
start$idle$1:
	xra	a			;4
	sta	recv$bit+2		;13
	lda	XxD$config		;13
	ani	1			;7
	adi	7			;7
	sta	RxD$count		;13
	lxi	h,que$full$test		;10
	shld	recv$state+1		;16
	ret				;10

	page
;
;	T states  57	RxD buffer not full
;	T states 117	RxD buffer full (send XOFF)
;	T states  86	RxD buffer full (XOFF sent already)
;
que$full$test:
	lxi	h,recv$bit		;10
	shld	recv$state+1		;16
	lda	RxD$buf$count		;13
	cpi	RxD$buf$size-16		;7
	rc				;5/11
	lxi	h,xon$xoff$flag		;10
	mvi	a,XOFF			;7
	cmp	m			;7
	rz				;5/11
	mov	m,a			;7	set mode to send Xoff
	mvi	a,send$x-send$norm	;7
	sta	send$idle+1		;13
	ret				;10

;
;	T states  64
;
recv$bit:
	rar				;4
	mvi	a,00			;7	RxD in carry bit
	rar				;4	move data bit into MSB
	sta	recv$bit+2		;13
	lxi	h,recv$bit$done$test	;10
	shld	recv$state+1		;16
	ret				;10

;
;	T states   69	if bits still remaining
;	T states  
;
recv$bit$done$test:
	lxi	h,RxD$count		;10
	dcr	m			;11
	jrnz	enter$recv$bit$idle	;7/(12+36)

	lda	XxD$config		;13
	rlc				;4
	lxi	h,enter$RxD$stop	;10
	jrnc	do$test$stop		;7/12
	lxi	h,enter$RxD$parity	;10
do$test$stop:
	shld	recv$state+1		;16

	ani	1*2			;7	A=0 if 7 bits else 8 bits
	lda	recv$bit+2		;13
	jrnz	done$adj		;7/12
	rrc				;4
done$adj:
	sta	RxD$data		;13
	ret				;10

;
;	T states  36
;
enter$recv$bit$idle:
	lxi	h,recv$bit$idle		;10
	shld	recv$state+1		;16
	ret				;10

;
;	T states  36+(28/105)
;
recv$bit$idle:
	lxi	h,recv$bit		;10
	shld	recv$state+1		;16
	jmp	RxD$unque		;10
	
	page
;
;	T states  36+(28/105)
;
enter$RxD$parity:
	lxi	h,test$RxD$parity	;10
	shld	recv$state+1		;16
	jmp	RxD$unque		;10

;
;	T states  	bit hi
;	T states  	bit low
;
test$RxD$parity:
	lxi	h,RxD$parity$idle	;10		RxD data bit in carry
	shld	recv$state+1		;16
	rar				;4
	lda	XxD$config		;13
	jrc	RxD$parity$hi		;7/12
	rlc				;4
	rlc				;4		mark space mode ? 
	jrnc	test$parity$space	;7/(12+15/46)	yes, go test it
	rlc				;4		get odd even mode
	jr	test$odd$even		;12+35/54

;
test$parity$space:		;15/
	rlc				;4
	rnc				;5/11
	jr	parity$error		;12+25

;
test$parity$mark:		;15/
	rlc				;4
	rc				;5/11
	jr	parity$error		;12+25

	page
;
RxD$parity$hi:				;4
	rlc				;4
	rlc				;4		mark/space mode ?
	jrnc	test$parity$mark	;7/12		yes, go test it
	rlc				;4		get odd/even flag
	cmc				;4		toggle it
test$odd$even:			;35/
	lda	recv$bit+2		;13
	aci	0			;7	
	ana	a			;4
	rpe				;5/11
parity$error:			;35
	lxi	h,RS232$status		;10
	setb	4,m			;15	set parity error
	ret				;10

;
;	T states  36
;
RxD$parity$idle:
	lxi	h,enter$RxD$stop	;10
	shld	recv$state+1		;16
	jmp	RxD$unque		;10

	page
;
;	T states   90	 if que not in use and DAV is cleared
;	T states 149/151 if data placed in que
;
enter$RxD$stop:
	lda	RS232$status		;13
	ani	00100001b		;7	DAV set or data in que?
	jrnz	place$in$que		;7/12	yes, place new char in que
	lda	RxD$data		;13	no, place char in data reg.
	sta	recv$data		;13
	lxi	h,test$RxD$stop$dav	;10
	shld	recv$state+1		;16
	ret				;10

;
place$in$que:			;116/118
	lxi	h,RxD$buf$count		;10
	inr	m			;11
	inr	l			;4
	mov	a,m			;7
	inr	a			;4
	cpi	buf$end			;7
	jrnz	put$buf$ok		;7/12
	mvi	a,low(RxD$buffer)	;7
put$buf$ok:
	mov	m,a			;7
	mov	l,a			;4
RxD$data	equ	$+1
	mvi	a,00			;7
	mov	m,a			;7
	lxi	h,test$RxD$stop$que	;10
	shld	recv$state+1		;16
	ret

	page
;
;	T states  	no errors
;	T states  	framing error	
;
test$RxD$stop$que:
	rar				;4
	mvi	a,00100000b		;7
	jmp	test$RxD$cont		;10

;
;	T states   	no errors
;	T states   	framing error	
;
test$RxD$stop$dav:
	rar				;4
	mvi	a,00000001b		;7
test$RxD$cont:
	jrc	good$RxD$stop		;7/12
	ori	00001000b		;7	set framing error
good$RxD$stop:
	lxi	h,RS232$status		;10
	ora	m			;7
	ani	11111101b		;7	clear recv active flag bit
	mov	m,a			;7
	lxi	h,recv$idle		;10
	shld	recv$state+1		;16
	ret				;10

	page
;*
;*	T states   	stay in idle state
;*	T states  	exit idle state (recv buffer not full)
;*	T states  	exit idle state (recv buffer full)
;*
send$idle:
	jr	send$norm		;12

send$norm:
	lda	RS232$status		;13
	rlc				;4
	rnc				;5/11

	lxi	h,start$send$1		;10
	shld	send$state+1		;16
	mvi	a,clear$TxD$bit		;7
	sta	out$rs232$cia		;13	send the start bit
	ret				;10

;
;	T states   12+118
;
send$x:
xon$xoff$flag	equ	$+1
	mvi	a,XON			;7	
	sta	send$bits+1		;13
	xra	a			;4
	sta	send$idle+1		;13
	mvi	a,clear$TxD$bit		;7
	sta	out$rs232$cia		;13	send the start bit
	lxi	h,RS232$status		;10
	setb	6,m			;15	flag send bussy
	lxi	h,start$xon$xoff	;10
	shld	send$state+1		;16
	ret				;10

	page

;
;	T states  107
;
start$send$1:
	lda	xmit$data		;13
	sta	send$bits+1		;13
	lxi	h,RS232$status		;10
	mov	a,m			;7
	xri	0C0h			;7	clear bit 7 and set bit 6
	mov	m,a			;7
start$xon$xoff:
	lda	XxD$config		;13
	ani	1			;7
	adi	7			;7
	sta	TxD$count		;13
enter$send$bits:
	lxi	h,start$bit$idle	;10
	shld	send$state+1		;16
	ret				;10

;
;	T states  36
;
start$bit$idle:
	lxi	h,send$bits		;10
	shld	send$state+1		;16
	ret				;10

;
;	T states  94	data bit low
;	T states  92	data bit hi	
;
send$bits:
	mvi	a,00			;7
	rrc				;4
	sta	send$bits+1		;13
	lxi	h,count$TxD		;10
	shld	send$state+1		;16
send$TxD:			;42/44
	mvi	a,set$TxD$bit		;7
	jrc	send$hi$bit		;7/12
	mvi	a,clear$TxD$bit		;7
send$hi$bit:
	sta	out$rs232$cia		;13
	ret				;10

;
;	T states  	if more data bits to send
;	T states  	if done sending bits
;
count$TxD:
	lxi	h,TxD$count		;10
	dcr	m			;11
	jrnz	enter$send$bits		;7/12
	lxi	h,TxD$parity$wait	;10
	shld	send$state+1		;16
	ret				;10

	page
;
;	T states  36
;
TxD$parity$wait:
	lxi	h,TxD$parity		;10
	shld	send$state+1		;16
	ret				;10

;
;
;	T states   85	if no parity
;	T states  124	if mark parity
;	T states  126	if space parity
;	T states  136	if even parity
;	T states  129	if odd parity
;
TxD$parity:
	lda	XxD$config		;13
	rlc				;4
	jrnc	TxD$stop		;7/(12+56)
	lxi	h,TxD$parity$idle$1	;10
	shld	send$state+1		;16
	rlc				;4
	jrnc	send$mark$space		;7/(12+16+42/44)
	rlc				;4
	lda	send$bits+1		;13
	aci	0			;7
	ana	a			;4
	mvi	a,set$TxD$bit		;7
	jpo	send$TxD$parity		;10
	mvi	a,clear$TxD$bit		;7
send$TxD$parity:
	sta	out$rs232$cia		;13
	ret				;10
;
send$mark$space:
	rlc				;4
	jr	send$TxD		;12+42/44

;
;	T states  36
;
TxD$parity$idle$1:
	lxi	h,TxD$parity$idle$2		;10
	shld	send$state+1			;16
	ret					;10

;
;	T states  36
;
TxD$parity$idle$2:
	lxi	h,TxD$stop			;10
	shld	send$state+1			;16
	ret					;10

	page
;
;	T states   103/101
;
TxD$stop:
	lxi	h,TxD$stop$idle			;10
	shld	send$state+1			;16

	mvi	a,set$TxD$bit			;7
	sta	out$rs232$cia			;13

	lda	XxD$config			;13
	ani	2				;7
	jrnz	one$stop$bit			;7/12
	mvi	a,5				;7
one$stop$bit:
	inx	h				;6
	mov	m,a				;7
	ret					;10

;
;	T states  35/90
;
TxD$stop$idle:
	mvi	a,00				;7
	dcr	a				;4
	sta	TxD$stop$idle+1			;13
	rnz					;5/11
	lxi	h,RS232$status			;10
	res	6,m				;15
	lxi	h,send$idle			;10
	shld	send$state+1			;16
	ret					;10
	
   endif
	page
;
;
;
Key$Scan$State:
	jr	scan$CIA		;12

;
;	T states	no new key down
;	T states	state change
;	T states  	new key down
;
scan$CIA:
	stc				;4
	mvi	a,11111110b		;7    data field updated by code
	lxi	b,key$row		;10
	outp	a			;12
	cpi	11111111b		;7
	jrz	extra$3			;7/12 carry=0 if A=11111111
	ral				;4
	sta	scan$CIA+1+1		;13
	lxi	h,key$scan$tbl		;10   get current new table pointer
	inr	c			;4    point to KEY$COL (input)
	jmp	cont$read		;10
;
extra$3:
	ral				;4
	sta	scan$CIA+1+1		;13
	mvi	a,scan$VIC-scan$CIA	;7
	sta	Key$Scan$State+1	;13
	ret				;10

	page
;
;	T states  	no new key and no state change
;
scan$VIC:
	mvi	a,11101110b		;7
	lxi	h,key$scan$tbl		;10   get current new table pointer
	lxi	b,vic$key$row		;10
	outp	a			;12	
	rlc				;4
	sta	scan$VIC+1		;13
	jrnc	normal$8		;7/12
	lxi	b,key$col		;10
cont$read:
	inp	a			;12   0FFh if no key down
	inr	m			;11
	mov	l,m			;7
	mov	b,m			;7    get old value in B
	mov	m,a			;7    save new value
	xra	b			;4    get differances
	ana	b			;4    test for only new keys down
	rz				;5/11
	sta	matrix$byte		;13
	lxi	h,key$found		;10
	shld	key$state+1		;16
	ret				;10
;
;
normal$8:
;	mvi	a,scan$CIA-scan$CIA	;7
	xra	a			;4
	sta	Key$Scan$State+1	;13
	mov	m,l			;7    reset current table pointer
	lxi	h,Key$Repeat$State	;10
	shld	key$state+1		;16
	ret				;10

	page
;
;	T states   48	repeat not active
;	T states  124	testing repeat (key realeased)
;	T states  110	testing repeat (not found)
;	T states  109	testing repeat (found)
;
Key$Repeat$State:
	lxi	h,flash$wait		;10
	shld	key$state+1		;16
repeat$count	equ	$+1
	mvi	a,00			;7
	ora	a			;4
	rz				;5/11
	lxi	h,repeat$count		;10
	dcr	a			;4    yes, test for repeat yet
	jrnz	not$repeat$yet		;7/(12+(42/56))
;
;	the following 4 lines of code may NOT be changed.
;	CONF.COM looks for them to change the repeat rate.
;	also looks for RET ; MVI A,xx ; STA xxxx (see set$key$parm)
;
	mvi	m,2			;10
	lxi	h,save$key		;10
	shld	key$state+1		;16
	ret				;10
;
not$repeat$yet:			;42/56
	mov	m,a			;7
matrix$pos	equ	$+1
	lda	Key$scan$tbl		;13
repeat$mask	equ	$+1
	mvi	b,00			;7
	ana	b			;4    key still down? (A=0)
	rz				;5/11 yes, exit for now
	mvi	m,0			;10
	ret				;10

	page
;
;	T states  101	flash
;	T states   72	no flash
;
flash$wait:
	mvi	a,01			;7
	dcr	a			;4
	sta	flash$wait+1		;13
	jrnz	no$flash		;7/(13+36)

	mvi	a,5			;7
	sta	flash$wait+1		;13
	lxi	h,flash			;10
	shld	key$state+1		;16	
	ret				;10

;*
;*
;*	T states 135	if cursor off screen
;*	T states 119	if cursor  on screen
;*
flash:
	lda	force$map		;13
	sta	bank$0			;13
	mov	b,a			;4
;
;	toggle 40 column screen cursor on/off
;
	lhld	flash$pos		;16
	xra	a			;4	clear A
	ora	h			;4	return if H=0
	jrz	exit$flash		;7/12
	mov	a,m			;7
	xri	80h			;7
	mov	m,a			;7
exit$flash:
	mov	a,b			;4
	sta	force$map		;13
no$flash:
	lxi	h,Key$Scan$State	;10
	shld	key$state+1		;16
	ret				;10

	page
;
;
;
key$found:	;148/138/157/147/166/156/161/167
matrix$byte	equ	$+1
	mvi	b,00			;7
	mov	a,b			;4
	ani	11110000b		;7
	jz	check$low$4		;10+(138/128/133/139)
	ani	11000000b		;7
	jrz	check$5$and$4		;7/(12+(110/100))
	ani	10000000b		;7
	mvi	c,6			;7
	jrnz	is$add$1		;7/(12+70)
	mvi	a,01000000b		;7
	jr	is$common		;12+66
;
check$5$and$4:			;110/100
	mov	a,b			;4
	ani	00100000b		;7
	mvi	c,4			;7
	jrnz	is$add$1		;7/(12+70)
	mvi	a,00010000b		;7
	jr	is$common		;12+66
;
;
check$low$4:		;138/128/133/139
	mov	a,b			;4
	ani	00001111b		;7
	jrz	exit$found		;7/12
	ani	00001100b		;7
	jrz	check$1$and$0		;7/12+(96/102)
	ani	00001000b		;7
	mvi	c,2			;7
	jrnz	is$add$1		;7/(12+70)
	mvi	a,00000100b		;7
	jr	is$common		;12+66
;
check$1$and$0:			;
	mov	a,b			;4
	ani	00000010b		;7
	mvi	c,0			;7
	jrnz	is$add$1		;7/(12+70)
	inr	a			;4    set A=1
	jr	is$common		;12+66
;
;
is$add$1:			;70
	inr	c			;4
is$common:			;66
	sta	mask$value		;13
	mov	a,c			;4
	sta	bit$value		;13
	lxi	h,key$found$2		;10
	shld	key$state+1		;16
	ret				;10
;
exit$found:
	lxi	h,Key$Scan$state	;10
	shld	key$state+1		;16
	ret				;10

	page
;
;	T states
;
key$found$2:
	lxi	h,matrix$byte		;10
mask$value	equ	$+1
	mvi	a,00			;7
	xra	m			;7   clear current bit
	mov	m,a			;7
bit$value	equ	$+1
	mvi	b,00			;7
	lxi	h,key$scan$tbl		;10  get the pointer
	mov	a,m			;7
	sub	l			;4   remove the bias
	dcr	a			;4   then one extra (pointer)
	add	a			;4
	add	a			;4
	add	a			;4   shift left 3 bits
	add	b			;4   add in bit position
	sta	key$code		;13  save as the key code
	lxi	h,remove$special$keys	;10
	shld	key$state+1		;16
	ret				;10

	page
;
;	T states  		if not a shift of control key
;	T states  68/82/96	if cntr / rt_shift / lf_shift
;
remove$special$keys:
	lxi	h,key$found		;10
	lda	key$code		;13
	cpi	38h+2			;7    control key pressed ?
	jrz	bad$key			;7/12
	cpi	30h+4			;7
	jrz	bad$key			;7/12
	cpi	08h+7			;7
	jrz	bad$key			;7/12

	lxi	h,set$key$parm		;10
bad$key:
	shld	key$state+1		;16
	ret				;10

;
;	T states
;
;	do not change the next 2 lines. CONF uses them to
;	the set repeat rate. (also RET above here)
;
set$key$parm:
	mvi	a,6			;7
	sta	repeat$count		;13  number of counts for repeat
	lda	key$scan$tbl		;13
	sta	matrix$pos		;13
	lda	mask$value		;13
	sta	repeat$mask		;13
	lxi	h,build$cntr$byte	;10
	shld	key$state+1		;16
	ret				;10

;
;	T states  
;
build$cntr$byte:
	lda	key$scan$tbl+1+7	;13  get control byte row
	cma				;4
	ani	04h			;7   test control key bit
	jrz	not$control		;7/12
	mvi	a,7			;7
not$control:
	mov	b,a			;4
	lda	key$scan$tbl+1+6	;13  get rigth shift byte row
	cma				;4
	ani	10h			;7   test right key bit
	ora	b			;4
	mov	b,a			;4
	lda	key$scan$tbl+1+1	;13  get left shift byte row
	cma				;4
	ani	80h			;7   test left key bit
	ora	b			;4
	mov	b,a			;4
	ani	90h			;7   either shift key down?
	mov	a,b			;4
	jrz	no$shift		;7/12 no, jump
	ori	2			;7    yes, set shift control bit
no$shift:
	sta	ctrl$byte		;13
	lxi	h,save$key		;10
	shld	key$state+1		;16
	ret				;10

	page
;
;
;	NOTE:	character buffer MUST be on one page
;		 (and have even number of bytes)
;
; buffer is FULL when data at put pointer does not equal 0ffh
; insert new character at (put pointer)
; and character control byte at (put pointer)+1
; 
;	T states 38 if buffer is full
;	T states 146/148
;
save$key:
	lhld	key$put$ptr		;16   get put pointer
	mov	a,m			;7    get byte from buffer
	inr	a			;4    room in buffer? (-1 if so)
	rnz				;5/11 no, wait for room in buffer
key$code	equ	$+1		
	mvi	m,00			;10   get matrix position
	inr	l			;4
ctrl$byte	equ	$+1
	mvi	a,00			;7
	mov	m,a			;7
	inr	l			;4
	mov	a,l			;4
	cpi	low(key$buffer+key$buf$size)	;7
	jrnz	put$ptr$ok		;7/12
	mvi	a,low(key$buffer)	;7
put$ptr$ok:
	sta	key$put$ptr		;13  adjust the low byte of the put ptr
	lxi	h,Key$tick		;10
	shld	key$state+1		;16
	ret				;10
;
;	T states
;
key$tick:
	lxi	b,sid+24		;10
	lda	tick$vol		;13
	outp	a			;12
	mvi	c,low(sid+11)		;7
	mvi	a,80h			;7
	outp	a			;12
	inr	a			;4
	outp	a			;12
	lxi	h,key$scan$state	;10
	shld	key$state+1		;16
	ret				;10

	page

;
;_____      _____ _____ _____ _____ _____ _____ _____ _____ _____ __________
;    |     |     |     |     |     |     |     |     |     |     |    |    |
;    |  S  |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  P  |   stop  |
;    |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|    |    |_
;
;
;  Reciever State Machine
;
;
;_____      _____ _____ _____ _____ _____ _____ _____ _____ _____ __________
;    |     |     |     |     |     |     |     |     |     |     |    |    |
;    |  S  |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  P  |   stop  |
;    |_____|_____|_____|_____|_____|_____|_____|_____|_____|_____|    |    |_
;
;
;  Transmitter State Machine (TSM)
;
;

;
;  Keyboard Scan State Machine (KSSM)
;

