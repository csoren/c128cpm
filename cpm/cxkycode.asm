	title	'CXKYCODE-  function and key def file   26 May 85'


	maclib	cxequ

number$blks	equ	4		; 256 byte blocks
def$per$key	equ	4
key$tbl$size	equ	11*8*def$per$key
color$tbl$size	equ	16

;
;	default Function keys and key definition
;
	org	sys$key$area

		dw	ascii$tbl-2

msgtbl:		db	'F1',0
		db	'F2',0
		db	'dir',cr,0
		db	'dir ',0
		db	'F5',0
		db	'F6',0
		db	'F7',0
		date
		db	5,18h,cr,0	; ^E ^X ^D
		db	'F9',0
		db	'F10',0
		db	'F11',0
		db	0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h
		db	0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0f3h,0
		db	0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h
		db	0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0f4h,0
		db	0f3h,0f3h,0f3h,0f3h,0
		db	0f4h,0f4h,0f4h,0f4h,0
		db	'F16',0
		db	'F17',0
		db	'F18',0
		db	'F19',0
		db	'F20',0
		db	'F21',0
		db	'F22',0
		db	'F23',0
		db	'F24',0
		db	'F25',0
		db	'F26',0
		db	'F27',0
		db	'F28',0
		db	'F29',0
		db	'F30',0
		db	'F31',0
		db	'Help ',0


msg$size	equ	$-msgtbl

	rept	(number$blks*256)-msg$size-key$tbl$size-color$tbl$size
		db	0ffh
	endm

	page

ascii$tbl:
	db	7fh,7fh,7fh,16h		; INS DEL
	db	0dh,0dh,0dh,0dh		; RETURN
	db	06h,06h,01h,01h		; LF RT
	db	86h,86h,87h,87h		; F7 F8
	db	80h,80h,81h,81h		; F1 F2
	db	82h,82h,83h,83h		; F3 F4
	db	84h,84h,85h,85h		; F5 F6
	db	17h,17h,17h,1ah		; UP DOWN

	db	33h,33h,23h,0A2h	; 3 #
	db	77h,57h,57h,17h		; W
	db	61h,41h,41h,01h		; A
	db	34h,34h,24h,0A3h	; 4 $
	db	7ah,5ah,5ah,1ah		; Z
	db	73h,53h,53h,13h		; S
	db	65h,45h,45h,05h		; E
	db	00h,00h,00h,00h		; (lf shift)

	db	35h,35h,25h,0A4h	; 5 %
	db	72h,52h,52h,12h		; R
	db	64h,44h,44h,04h		; D
	db	36h,36h,26h,0A5h	; 6 &
	db	63h,43h,43h,03h		; C
	db	66h,46h,46h,06h		; F
	db	74h,54h,54h,14h		; T
	db	78h,58h,58h,18h		; X

	db	37h,37h,27h,0A6h	; 7 '
	db	79h,59h,59h,19h		; Y
	db	67h,47h,47h,07h		; G
	db	38h,38h,28h,0A7h	; 8 (
	db	62h,42h,42h,02h		; B
	db	68h,48h,48h,08h		; H
	db	75h,55h,55h,15h		; U
	db	76h,56h,56h,16h		; V

	db	39h,39h,29h,00h		; 9 )
	db	69h,49h,49h,09h		; I
	db	6ah,4ah,4ah,0ah		; J
	db	30h,30h,30h,00h		; 0
	db	6dh,4dh,4dh,0dh		; M
	db	6bh,4bh,4bh,0bh		; K
	db	6fh,4fh,4fh,0fh		; O
	db	6eh,4eh,4eh,0eh		; N

	db	2bh,2bh,2bh,00h		; +
	db	70h,50h,50h,10h		; P
	db	6ch,4ch,4ch,0ch		; L
	db	2dh,2dh,2dh,00h		; -
	db	2eh,2eh,3eh,00h		; . >
	db	3ah,3ah,5bh,7bh		; : [ {
	db	40h,40h,40h,00h		; @
	db	2ch,2ch,3ch,00h		; , <

	db	23h,23h,23h,60h		; pound `
	db	2ah,2ah,2ah,00h		; *
	db	3bh,3bh,5dh,7dh		; ; ] }
	db	00h,00h,00h,0f5h	; clear/home
	db	00h,00h,00h,00h		; (rt shift)
	db	3dh,3dh,3dh,7eh		; = ~
	db	5eh,5eh,7ch,7ch		; ^ PI |
	db	2fh,2fh,3fh,5ch		; / ? \

	db	31h,31h,21h,0A0h	; 1
	db	5fh,5fh,5fh,7fh		; <-
	db	09h,15h,30h,00h		; (CONTROL) sound1 sound2
	db	32h,32h,22h,0A1h	; 2 "
	db	20h,20h,20h,00h		; Space
	db	21h,20h,00h,00h		; (Commodore) sound3
	db	71h,51h,51h,11h		; Q
	db	00h,00h,00h,0f0h	; RUN STOP

	db	9fh,9fh,9fh,9fh		; /HELP/
	db	38h,38h,38h,0B7h	; /8/ 
	db	35h,35h,35h,0B4h	; /5/
	db	09h,09h,09h,00h		; /TAB/
	db	32h,32h,32h,0B1h	; /2/
	db	34h,34h,34h,0B3h	; /4/
	db	37h,37h,37h,0B6h	; /7/
	db	31h,31h,31h,0B0h	; /1/

	db	1bh,1bh,1bh,00h		; /ESC/
	db	2bh,2bh,2bh,0F7h	; /+/   (select VT100)
	db	2dh,2dh,2dh,0F6h	; /-/	(select ADM31)
	db	0Ah,0Ah,0Ah,0Ah		; /Line Feed/
	db	0dh,0dh,0dh,0ffh	; /ENTR/
	db	36h,36h,36h,0B5h	; /6/
	db	39h,39h,39h,00h		; /9/
	db	33h,33h,33h,0B2h	; /3/

	db	00h,00h,00h,00h		; /Alt/
	db	30h,30h,30h,00h		; /0/
	db	2eh,2eh,2eh,00h		; /./
	db	05h,05h,05h,12h		; /UP/
	db	18h,18h,18h,03h		; /DN/
	db	13h,13h,13h,08dh	; /LF/
	db	04h,04h,04h,08eh	; /RT/
	db	0f1h,0f1h,0f1h,0f2h	; /no scroll/

;
;	logical color table (used with ESC ESC ESC char)
;				(where char is 50h to 7fh)
;
	db	000h,011h,022h,033h
	db	044h,055h,066h,077h
	db	088h,099h,0aah,0bbh	
	db	0cch,0ddh,0eeh,0ffh

