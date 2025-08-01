BasicUpstart2(start)

* = $0810 "Main Program"

     // load charset to $2000
     // sprite at       $2800
     // load screen to  $3000

     // BOUNDARY SET CAN BE DONE
     // VIA BIT MASKING!!!!!!
start: 
.const sprpos   = $d000

.const pra      = $dc00 //port reg a
.const ddra     = $dc02 //data direcuion reg a

.const prb      = $dc01 //port reg b
.const ddrb     = $dc03 //data direction reg b

.const penlo    = $fb   //used for
.const penhi    = $fc   // drawing

.const chrlo    = $03   //used for keeping track
.const chrhi    = $04   // of active chr mem loc

	lda #147    //clear screen
	jsr $ffd2
	lda #$04
	sta $d020   //border color
	lda #$00
	sta $d021   //screen color


	lda #%00011000
	sta $d018   //mem config

	lda #$24
	sta chrhi   //chrset at $2400

	ldx #$00
	jsr drawscreen

	lda #$e1
	sta penlo
	lda #$05
	sta penhi
	ldx #128  //char at $2400
	ldy #0
	jsr drawtileset

	ldx #$00
	ldy #7
	jsr drawchar

	//sprite setup
	lda #%00000011
	sta $d015
	lda #$a0      // $2800
	sta $07f8
	sta $07f9
	lda #5        // green
	sta $d027     // color
	sta $d028

	jsr placecursor

	jsr drawcolindex //color indx

	jsr modified
	jsr displaytiles
	
forever:
	jsr raster
	jsr input
	jmp forever

	//---------------------------------------
displaytiles:
	lda #$05
	sta penhi
	lda #0
	sta tilecounter

tileloop:
	ldx tilecounter
	lda tilepos,x
	sta penlo
	lda #$05
	sta penhi
	jsr drawtile
	inc tilecounter
	lda tilecounter
	cmp #10
	bne tileloop

	lda #$06
	sta penhi
	lda #$6f
	sta penlo
	lda #0
	sta counter
	jsr disptiledata

	rts

	//---------------------------------------
drawtile:

	ldx #0
	ldy #$00
	lda tilechars
	sta (penlo),y
	lda #$d9
	sta penhi
	lda tilecolors,x
	sta (penlo),y

	lda #5
	sta penhi
	lda #1
	jsr incpen
	inx
	lda tilechars,x
	sta (penlo),y
	lda #$d9
	sta penhi
	lda tilecolors,x
	sta (penlo),y

	lda #5
	sta penhi
	lda #$27
	jsr incpen
	inx
	lda tilechars,x
	sta (penlo),y
	lda #$d9
	sta penhi
	lda tilecolors,x
	sta (penlo),y

	lda #5
	sta penhi
	lda #1
	jsr incpen
	inx
	lda tilechars,x
	sta (penlo),y
	lda #$d9
	sta penhi
	lda tilecolors,x
	ldy #0
	sta (penlo),y

	rts

	//---------------------------------------
tilechars: .byte $80,$81,$82,$83
tilecolors: .byte $05,$03,$03,$05
tilecounter: .byte 0
tilepos:  .byte $39
	.byte $30,$32,$34
	.byte $80,$82,$84
	.byte $d0,$d2,$d4


	//---------------------------------------
setcrsrdisplay:
	// set correct pos to place

	// char edit cursor
	lda cursorx
	clc
	adc canvasx
	sta crsrdpx
	lda cursory
	clc
	adc canvasy
	sta crsrdpy

	// charset cursor
	lda csetcrsrx
	clc
	adc csetcanvasx
	sta csetcrsrdpx
	lda csetcrsry
	clc
	adc csetcanvasy
	sta csetcrsrdpy

	rts

	//---------------------------------------
placecursor:

	jsr setcrsrdisplay
	jsr placecsetcrsr
	// y before x
	lda crsrdpy
	pha
	lda crsrdpx
	pha
	lda csetcrsrdpy
	pha
	lda csetcrsrdpx
	pha
	ldx #0

placecursorloop:
	pla
	asl 
	asl 
	asl  // * 8
	clc
	adc #23     //border size - 1
	sta sprpos,x

	pla
	asl 
	asl 
	asl  // * 8
	clc
	adc #49     //border size - 1
	sta sprpos+1,x
	inx
	inx
	cpx #$04
	bmi placecursorloop
	rts

	//---------------------------------------
placecsetcrsr:
	// place character set cursor
	// based directly on chrindex value

	// cursors get redrawn at
	//   capchrindex

	// get y
	lda chrindex
	and #%11110000
	lsr 
	lsr 
	lsr 
	lsr 
	clc
	adc csetcanvasy
	sta csetcrsrdpy

	// get x
	lda chrindex
	and #%00001111
	clc
	adc csetcanvasx
	sta csetcrsrdpx
	rts


	//---------------------------------------
printhex:

	ldy #0
	pha
	and #%11110000
	lsr 
	lsr 
	lsr 
	lsr 
	tax
	lda hexlookup,x
	sta (penlo),y

	lda #1
	jsr incpen
	pla
	and #%00001111
	tax
	lda hexlookup,x
	sta (penlo),y

	rts


	//---------------------------------------
hexlookup:
	.byte $30,$31,$32,$33,$34,$35
	.byte $36,$37,$38,$39
	.byte $01,$02,$03,$04,$05,$06
	//---------------------------------------
getchrmem:
	// get active char memory location

	lda #0
	sta chrlo
	// lda #$24
	sta chrhi

	lda chrindex
	asl     // * 2
	rol chrhi //rotate to hi-byte
	asl     // * 4
	rol chrhi
	asl     // * 8
	rol chrhi
	sta chrlo

	lda chrhi
	clc
	adc #$24
	sta chrhi
	rts

	//---------------------------------------
drawchar:
	// $0429 first pixel
	// ldx #0
	// ldy #7

	lda #$29
	sta penlo
	lda #$04
	sta penhi

	jsr getchrmem

drawcharloop:
	sty counter
	lda #$7f       // px on
	sta chartodraw
	ldy #0
	lda (chrlo),y
	ldy counter
	and bitmask,y
	bne nopxerase
	lda #$20       // px off?
	sta chartodraw

nopxerase:
	// display pixel
	tya
	pha
	lda chartodraw
	ldy #0
	sta (penlo),y
	// color char:
	lda penhi
	pha
	clc
	adc #$d4
	sta penhi
	lda colindex
	sta (penlo),y
	pla
	sta penhi
	pla
	tay

drawdone:
	lda #1
	jsr incpen
	// check if row complete
	dey
	cpy #$ff
	bne drawcharloop

	ldy #7
	lda #$20
	jsr incpen  //move pen
	inc chrlo
	inx
	stx counter
	cpx #$08    //check if complete
	bne drawcharloop
	rts

	//---------------------------------------
drawtileset:
	// y = row index
	// tileset loc = $05e1

	tya
	pha
	txa
	sta (penlo),y
	pla
	tay

	lda #0
	//     sta incval
	jsr incpen

	iny
	tya
	and #%00010000
	beq skiprowinc
	ldy #0
	lda #$28      // inc row
	//    sta incval
	jsr incpen

skiprowinc:
	inx
	bne drawtileset
	rts


	//---------------------------------------
drawcolindex:
	// arrow chr = #121
	// first pos = $07c0

	// this routine can be crunched

	lda #$07
	sta penhi
	ldy #0

	// erase last chr:
	lda lastcolindex
	asl 
	clc
	adc #$c0
	sta penlo

	lda #$20        //empty chr
	sta (penlo),y   //delete

	// draw new one
	lda colindex
	asl         // * 2
	clc
	adc #$c0
	sta penlo

	lda #121
	sta (penlo),y

	lda colindex     // update
	sta lastcolindex //
	rts

	//---------------------------------------

drawscreen:
	lda $3000,x
	sta $0400,x
	lda $3100,x
	sta $0500,x
	lda $3200,x
	sta $0600,x
	lda $32e8,x
	sta $06e8,x
	inx
	bne drawscreen

colorscreen:
	lda $33e8,x
	sta $d800,x
	lda $34e8,x
	sta $d900,x
	lda $35e8,x
	sta $da00,x
	lda $36d0,x
	sta $dae8,x
	dex
	bne colorscreen
	rts

	//---------------------------------------
setpixel1:
	ldx cursory
	dex
	stx tempcursory // for offset
	ldx cursorx   // use as bitmask
	ldy revbitmask,x
	sty temp
	ldy tempcursory  // use as index
	lda (chrlo),y // row of pixels
	bit temp      // is px on?
	beq noerase
	ldy cursorx
	lda revinvbitmask,y
	ldy tempcursory
	and (chrlo),y // turn px off
	sta (chrlo),y
	jmp pxdone

setpixel:
	jsr getchrmem

	ldy cursory      //index to byte
	lda (chrlo),y
	ldx cursorx      //indx to bit
	ldy revbitmask,x
	sty temp
	bit temp
	beq noerase
	ldx cursorx
	lda revinvbitmask,x
	ldy cursory
	and (chrlo),y
	sta (chrlo),y
	jmp pxdone

noerase:
	ldy cursory
	ora temp
	sta (chrlo),y
pxdone:
	ldx #0
	ldy #7
	jsr drawchar
	rts

	//---------------------------------------
clearchar:
	jsr getchrmem
	lda #0
	ldy #8

clrchrloop:
	dey
	sta (chrlo),y
	cpy #0
	bne clrchrloop

	ldx #0
	ldy #7
	jsr drawchar
	rts

	//---------------------------------------
displaydata:
	ldy counter
	lda (chrlo),y
	jsr printhex
	lda #$03
	jsr incpen
	inc counter

	ldy counter
	lda (chrlo),y
	jsr printhex
	lda #$23
	jsr incpen
	inc counter

	lda counter
	cmp #$08
	bne displaydata

	rts

	//---------------------------------------
disptiledata:
	// display which tiles appear
	// in preview

	ldx counter
	lda tilechars,x
	jsr printhex
	lda #3
	jsr incpen
	inc counter
	lda counter
	cmp #$04
	bne disptiledata
	rts


	//---------------------------------------
modified:
	// jump here when data is
	// modified. update screen
	// accordingly

	jsr getchrmem


	lda #$04
	sta penhi
	lda #$35
	sta penlo
	ldx #0
	stx counter
	jsr displaydata



	// change index
	lda #$05
	sta penhi
	lda #$4d
	sta penlo
	//      lda chrhi
	//      jsr printhex

	//      lda #$01
	//      jsr incpen
	//      lda chrlo
	//      jsr printhex

	lda chrindex
	jsr printhex

	ldx #0
	ldy #7
	jsr drawchar


	rts

	//---------------------------------------
capchrindex:
	// set chrindex max to 127
	lda chrindex
	and #%01111111
	sta chrindex

	jsr placecursor   //!!!!!

	rts

	//---------------------------------------
incpen:
	clc
	adc penlo
	sta penlo
	lda penhi
	adc #$00
	sta penhi
	rts

	//---------------------------------------
raster:
	lda #$ff
	cmp $d012
	bne raster
	rts

	//- variables ---------------------------

temp:     .byte 0
tempcursory: .byte 0  //for setpixel
counter:  .byte 0
//incval   .byte 0    //pen inc amount
chrindex: .byte $00  //active char
colindex: .byte $01  //active color
lastcolindex: .byte $01

canvasx:  .byte $01  // drawing
canvasy:  .byte $01  //  area
cursorx:  .byte $00
cursory:  .byte $00
crsrdpx:  .byte $00  // actual cursor
crsrdpy:  .byte $00  // display location

csetcanvasx: .byte $01 // character set
csetcanvasy: .byte $0c //  location
csetcrsrx: .byte $00
csetcrsry: .byte $00
csetcrsrdpx: .byte $00 //  on the screen
csetcrsrdpy: .byte $00 //

crsrdelta: .byte 0,0 //x, y crsr movement

chartodraw: .byte 0 // $7f = px on,
		   // $32 = px off


	//---------------------------------------

input:
	sei

	// keyboard

	// prepare keyboard
	lda #%11111111
	sta ddra
	lda #%00000000
	sta ddrb

num1key:
	// 1
	// set tile chr 0
	lda #%01111111
	sta pra
	lda #%00000001
	bit prb
	bne nonum1key
	lda inputdata1
	and #%00010000
	bne num2key
	lda #%00010000
	ora inputdata1
	sta inputdata1
	lda chrindex
	ora #%10000000
	sta tilechars
	//  jsr displaytiles
	//  jsr lshift
	//  bne num2key
	lda colindex
	sta tilecolors
	jsr displaytiles
	jmp num2key
nonum1key:
	lda inputdata1
	and #%11101111
	sta inputdata1
num2key:
	lda #%01111111
	sta pra
	lda #%00001000
	bit prb
	bne nonum2key
	lda inputdata1
	and #%00100000
	bne num3key
	lda #%00100000
	ora inputdata1
	sta inputdata1
	lda chrindex
	ora #%10000000
	sta tilechars+1
	//  jsr displaytiles
	//  jsr lshift
	//  bne nonum2key
	lda colindex
	sta tilecolors+1
	jsr displaytiles
	jmp num3key
nonum2key:
	lda inputdata1
	and #%11011111
	sta inputdata1
num3key:
	lda #%11111101
	sta pra
	lda #%00000001
	bit prb
	bne nonum3key
	lda inputdata1
	and #%01000000
	bne num4key
	lda #%01000000
	ora inputdata1
	sta inputdata1
	lda chrindex
	ora #%10000000
	sta tilechars+2
	lda colindex
	sta tilecolors+2
	jsr displaytiles
	jmp num4key
nonum3key:
	lda inputdata1
	and #%10111111
	sta inputdata1
num4key:
	lda #%11111101
	sta pra
	lda #%00001000
	bit prb
	bne nonum4key
	lda inputdata1
	and #%10000000
	bne wkey
	lda #%10000000
	ora inputdata1
	sta inputdata1
	lda chrindex
	ora #%10000000
	sta tilechars+3
	lda colindex
	sta tilecolors+3
	jsr displaytiles
	jmp wkey
nonum4key:
	lda inputdata1
	and #%01111111
	sta inputdata1
wkey:

	// w
	// chrset up
	lda #%11111101
	sta pra
	lda #%00000010
	bit prb
	bne nowkey
	lda inputdata1
	and #%00001000
	bne skey
	lda #%00001000
	ora inputdata1
	sta inputdata1
	jsr chrmoveu
	jmp skey
nowkey:
	lda inputdata1
	and #%11110111
	sta inputdata1
skey:
	// s
	// chrset down
	lda #%11111101
	sta pra
	lda #%00100000
	bit prb
	bne noskey
	lda inputdata1
	and #%00000100
	bne akey
	lda #%00000100
	ora inputdata1
	sta inputdata1
	jsr chrmoved
	jmp akey
noskey:
	lda inputdata1
	and #%11111011
	sta inputdata1
akey:
	// a
	// chrset crsr left
	lda #%11111101
	sta pra
	lda #%00000100
	bit prb
	bne noakey
	lda inputdata1
	and #%00000010
	bne dkey
	lda #%00000010
	ora inputdata1
	sta inputdata1
	jsr chrmovel
	jmp dkey
noakey:
	lda inputdata1
	and #%11111101
	sta inputdata1
dkey:
	// d
	// charset cursor right
	lda #%11111011
	sta pra
	lda #%00000100
	bit prb
	bne nodkey
	lda inputdata1
	and #%00000001
	bne delkey
	lda #%00000001
	ora inputdata1
	sta inputdata1
	jsr chrmover
	jmp delkey
nodkey:
	lda inputdata1
	and #%11111110
	sta inputdata1
delkey:
	// inst/del
	// clear current char
	lda #%11111110
	sta pra
	lda #%00000001
	bit prb
	bne nodelkey
	lda inputdata
	and #%10000000
	bne spacekey
	lda #%10000000
	ora inputdata
	sta inputdata
	jsr clearchar
	jsr modified
	jmp spacekey

nodelkey:
	lda inputdata
	and #%01111111
	sta inputdata

spacekey:
	// spacebar
	// draw pixel
	lda #%01111111
	sta pra
	lda #%00010000
	bit prb
	bne nospacekey
	lda #%01000000
	and inputdata
	bne f1key
	lda #%01000000
	ora inputdata
	sta inputdata
	jsr setpixel
	jsr modified
	jmp f1key

nospacekey:
	lda inputdata
	and #%10111111
	sta inputdata

f1key:
	// f1
	// move color index <-
	lda #%11111110
	sta pra
	lda #%00010000
	bit prb
	bne nof1
	lda #%00010000
	and inputdata
	bne f3key
	lda inputdata
	ora #%00010000
	sta inputdata
	dec colindex
	jsr setcolindex
	jsr drawcolindex
	jmp f3key
nof1:
	lda inputdata
	and #%11101111
	sta inputdata

f3key:
	// f3
	// move color index ->
	lda #%11111110
	sta pra
	lda #%00100000
	bit prb
	bne nof3
	lda #%00100000
	and inputdata
	bne shiftr
	lda inputdata
	ora #%00100000
	sta inputdata
	inc colindex
	jsr setcolindex
	jsr drawcolindex
	jmp shiftr
nof3:
	lda inputdata
	and #%11011111
	sta inputdata

shiftr:
	// shift + crsr r
	lda #%10111110 // 1.
	sta pra        // mask
	lda #%00010100 // shift+crsr r
	bit prb
	bne noshiftr
	lda #%00000010 // 2. pressed
	and inputdata  // this frame?
	bne shiftd
	lda inputdata  // 3.
	ora #%00000010 // set input
	sta inputdata  // bit
	jsr moveleft   // 4. function
	jmp noinput    // 5. skip check
				// for key
				// without shift
noshiftr:
	lda inputdata  // 6. if not
	and #%11111101 // pressed then
	sta inputdata  // deactivate
				// input bit

rkey:
	// crsr r
	lda #%11111110
	sta pra
	lda #%00000100
	bit prb
	bne norkey
	lda #%00000001
	and inputdata
	bne shiftd
	lda inputdata
	ora #%00000001
	sta inputdata
	jsr moveright
	jmp noinput
norkey:
	lda inputdata
	and #%11111110
	sta inputdata

shiftd:
	// rshift + crsr down
	lda #%10111110
	sta pra
	lda #%10010000
	bit prb
	bne noshiftd
	lda #%00001000
	and inputdata
	bne noinput
	lda inputdata
	ora #%00001000
	sta inputdata
	jsr moveup
	jmp noinput
noshiftd:
	lda inputdata
	and #%11110111
	sta inputdata

downkey:
	lda #%11111110
	sta pra
	lda #%10000000
	bit prb
	bne nodown
	lda #%00000100
	and inputdata
	bne noinput
	lda #%00000100
	ora inputdata
	sta inputdata
	jsr movedown
	jmp noinput

nodown:
	lda inputdata
	and #%11111011
	sta inputdata

noinput:
	//  lda pra
	//  sta lastinput

	cli
	rts

lshift:
	lda #%11111101
	sta pra
	lda #%10000000
	and prb
	rts

	//---------------------------------------
inputdata: .byte $00
	// bits:
	// 0 - cursor right
	// 1 - cursor r + shift
	// 2 - cursor down
	// 3 - cursor d + shift
	// 4 - f1
	// 5 - f3
	// 6 - space
	// 7 - del (clear)

inputdata1: .byte $00
	// bits:
	// 0 - d
	// 1 - a
	// 2 - s
	// 3 - w
	// 4 - 1
	// 5 - 2
	// 6 - 3
	// 7 - 4

lastinput: .byte 0 //for single frame
		  // keypresses

	//---------------------------------------
moveright:
	inc cursorx
	lda cursorx
	and #%00000111
	sta cursorx
	jsr placecursor
	rts
moveleft:
	dec cursorx
	lda cursorx
	and #%00000111
	sta cursorx
	jsr placecursor
	rts
movedown:
	inc cursory
	lda cursory
	and #%00000111
	sta cursory
	jsr placecursor
	rts
moveup:
	dec cursory
	lda cursory
	and #%00000111
	sta cursory
	jsr placecursor
	rts

	// character set: - - - - - - - - -

chrmover:  // move right
	inc chrindex
	jsr capchrindex
	jsr modified
	rts

chrmovel:  // left
	dec chrindex
	jsr capchrindex
	jsr modified
	rts

chrmoved:  // down
	lda chrindex
	clc
	adc #16
	sta chrindex
	jsr capchrindex
	jsr modified
	rts

chrmoveu:  // up
	lda chrindex
	sec
	sbc #16
	sta chrindex
	jsr capchrindex
	jsr modified
	rts

setcolindex:

	lda colindex
	and #%00001111
	sta colindex

	lda colindex
	clc
	adc #$01       //different
	sta $d028      // crsr color

	ldx #0
	ldy #7
	jsr drawchar

	// change bg to gray if col = 0
	lda #$00
	sta $d021
	lda colindex
	bne noblack
	lda #11
	sta $d021
noblack:
	rts

* = $2000 "Charset"
	
	.import binary "charset.chr"
	
	//- sprite ------------------------------
* = $2800 "Sprite"

	.byte $cc,$c0,$00,$80,$40,$00
	.byte $00,$00,$00,$00,$00,$00
	.byte $80,$40,$00,$80,$40,$00
	.byte $00,$00,$00,$00,$00,$00
	.byte $80,$40,$00,$cc,$c0,$00

	.byte $00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00


	//---------------------------------------
bitmask:
	.byte %00000001
	.byte %00000010
	.byte %00000100
	.byte %00001000
	.byte %00010000
	.byte %00100000
	.byte %01000000
	.byte %10000000

invbitmask:
	.byte %11111110
	.byte %11111101
	.byte %11111011
	.byte %11110111
	.byte %11101111
	.byte %11011111
	.byte %10111111
	.byte %01111111

revbitmask:
	.byte %10000000
	.byte %01000000
	.byte %00100000
	.byte %00010000
	.byte %00001000
	.byte %00000100
	.byte %00000010
	.byte %00000001

revinvbitmask:
	.byte %01111111
	.byte %10111111
	.byte %11011111
	.byte %11101111
	.byte %11110111
	.byte %11111011
	.byte %11111101
	.byte %11111110


*=$3000 "Screen"
	.import binary "screen.bin"