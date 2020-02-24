Class {
	#name : #Forth,
	#superclass : #Object,
	#instVars : [
		'memory',
		'stack',
		'returnStack',
		'pc',
		'pcNext',
		'pcCurrent',
		'initPos',
		'labels',
		'lastWord',
		'inputBuffer',
		'outputBuffer',
		'unknownLabels',
		'state',
		'keyboardBuffer'
	],
	#category : #'Phortissimo-Core-Basic'
}

{ #category : #accessing }
Forth class >> plotAscii [

<script>
| forth |
forth := ForthUppercase new init.
forth openPlotter.
forth loadStd.

forth << '

: variable: VARIABLE LATEST @ >CFA EXECUTE ! ;
: ->cell 4 SWAP +! ;
: <-cell 4 SWAP -! ;

32 CELLS ALLOT variable: loopSP
loopSP @ CONSTANT loopTop
: >loop loopSP @ ! loopSP ->cell ;
: loop> loopSP <-cell loopSP @ @ ; 
: do IMMEDIATE '' >loop , '' >loop , [COMPILE] BEGIN ;
: loopCheck loop> loop> 1+  2DUP =  -ROT >loop >loop ;
: loopFinish loop> DROP loop> DROP ;
: loop IMMEDIATE '' loopCheck , [COMPILE] UNTIL '' loopFinish , ;

: mm 10 * ;
: cm 100 * ;
: dot ;
: dots ;
: pt 1 dots * ;

800 VALUE maxPlotterX 
300 VALUE maxPlotterY

VARIABLE plotterX
0 plotterX !
VARIABLE plotterY
0 plotterY !

: penTranslate BEGIN DUP 0> WHILE PENSTEP 1- REPEAT DROP ;
: penMove PENUP penTranslate ;
: penDraw PENDOWN penTranslate ;

: abs DUP 0< IF DUP DUP + - THEN ;
 
: PENWEST PENLEFT ; 
: PENEAST PENRIGHT ;

: validPlotterX DUP 0>= SWAP maxPlotterX <= AND ;
: validPlotterY DUP 0>= SWAP maxPlotterY <= AND ;

: validatePlotterY plotterY @ SWAP pt + validPlotterY ;
: updatePlotterY pt plotterY +! ;
: validatePlotterX plotterX @ SWAP pt + validPlotterX ;
: updatePlotterX pt plotterX +! ;
: drawPt 1 pt penDraw ;
: movePt 1 pt penMove ;

: setDirectionNorth -1 validatePlotterY DUP IF PENNORTH -1 updatePlotterY THEN ;
: setDirectionSouth  1 validatePlotterY DUP IF PENSOUTH  1 updatePlotterY THEN ;
: setDirectionEast   1 validatePlotterX DUP IF PENEAST   1 updatePlotterX THEN ;
: setDirectionWest  -1 validatePlotterX DUP IF PENWEST  -1 updatePlotterX THEN ;

: drawIfPossible IF drawPt THEN ;
: moveIfPossible IF movePt THEN ;
: drawIfBothPossible OR IF drawPt THEN ;
: moveIfBothPossible OR IF movePt THEN ;

: drN  PENRESET setDirectionNorth drawIfPossible ;
: drS  PENRESET setDirectionSouth drawIfPossible ;
: drW  PENRESET setDirectionWest  drawIfPossible ;
: drE  PENRESET setDirectionEast  drawIfPossible ;
: drNE PENRESET setDirectionNorth setDirectionEast drawIfBothPossible ;
: drNW PENRESET setDirectionNorth setDirectionWest drawIfBothPossible ;
: drSE PENRESET setDirectionSouth setDirectionEast drawIfBothPossible ;
: drSW PENRESET setDirectionSouth setDirectionWest drawIfBothPossible ;

: moveN  PENRESET setDirectionNorth moveIfPossible ;
: moveS  PENRESET setDirectionSouth moveIfPossible ;
: moveW  PENRESET setDirectionWest  moveIfPossible ;
: moveE  PENRESET setDirectionEast  moveIfPossible ;	
: moveNE PENRESET setDirectionNorth setDirectionEast moveIfBothPossible ;
: moveNW PENRESET setDirectionNorth setDirectionWest moveIfBothPossible ;
: moveSE PENRESET setDirectionSouth setDirectionEast moveIfBothPossible ;
: moveSW PENRESET setDirectionSouth setDirectionWest moveIfBothPossible ;

: penJump 
   DUP 0 > IF DUP 0 do moveS loop THEN
   DUP 0 < IF DUP abs 0 do moveN loop THEN 
   DROP
   DUP 0 > IF DUP 0 do moveE loop THEN
   DUP 0 < IF DUP abs 0 do moveW loop THEN
 	DROP
	;

: letterOffset moveE ;

: letter1 
	1 2 penJump
	drNE drS drS drS drS drS drS drW drE drE
	1 -7 penJump
	letterOffset ;
: letter2
	0 2 penJump
	drNE drE drE drSE drS drSW drW drW drSW drS drS drE drE drE drE
	0 -7 penJump
	letterOffset ;
: letter3
	0 2 penJump
	drNE drE drE drSE drS drSW drW drE drSE drS drSW drW drW drNW
	4 -6 penJump
letterOffset ;
: letter4
	4 5 penJump
	drW drW drW drW drN drNE drNE drNE drS drS drS drS drS drS
	1 -7 penJump
	letterOffset ;
: letter5
	4 1 penJump
	drW drW drW drW drS drS drE drE drE drSE drS drS drSW drW drW drNW
	4 -6 penJump
	letterOffset ;
: letter6
	3 1 penJump
	drW drSW drSW drS drS drS drSE drE drE drNE drN drNW drW drW drW
	4 -4 penJump
	letterOffset ;
: letter7
	0 1 penJump
	drE drE drE drE drSW drSW drSW drS drS drS
	3 -7 penJump
	letterOffset ;		
: letter8
	0 2 penJump
	drS drSE drE drE drSE drS drSW drW drW drNW drN drNE drE drE drNE drN drNW drW drW drSW
	4 -2 penJump
	letterOffset ;	
: letter9
	4 4 penJump
	drW drW drW drNW drN drNE drE drE drSE drS drS drS drSW drSW drW
	3 -7 penJump
	letterOffset ;
	
: letter0
	0 6 penJump
	drSE drE drE drNE drN drN drN drN drNW drW drW drSW drS drS drS drS drNE drNE drNE drNE
	0 -2 penJump
	letterOffset ;	
	
: letterA
	 0 7 penJump
	drN drN drN drN drN drNE drE drE drSE drS drS drS drS drS drN drN drN drW drW drW drW
	4 -4 penJump
	letterOffset ;
: lettera
	 3 6 penJump
	drSW drW drNW drN drN drNE drE drSE drS drS drSE
	0 -7 penJump
	letterOffset ;		
: letterB
	0 1 penJump
	drE drE drE drSE drS drSW drW drW drE drE drSE drS drSW drW drW drW drE drN drN drN drN drN drN
	3 -1 penJump
	letterOffset ;
: letterb
	0 1 penJump
	drS drS drS drS drS drS drN drN drSE drSE drE drNE drN drN drNW drW drSW drSW
	4 -5 penJump
	letterOffset ;
: letterC
	4 2 penJump
	drNW drW drW drSW drS drS drS drS drSE drE drE drNE
	0 -6 penJump
	letterOffset
	;
: letterc
	4 7 penJump
	drW drW drW drNW drN drN drNE drE drE drE
	0 -3 penJump
	letterOffset
	;
: letterD
	0 1 penJump
	drE drE drE drSE drS drS drS drS drSW drW drW drW drE drN drN drN drN drN drN
	3 -1 penJump
	letterOffset
	;
: letterd
	4 5 penJump
	drNW drNW drW drSW drS drS drSE drE drNE drNE drS drS drN drN drN drN drN drN
	0 -1 penJump
	letterOffset
	;
: letterE
	4 1 penJump
	drW drW drW drW drS drS drS drE drE drW drW drS drS drS drE drE drE drE
	0 -7 penJump
	letterOffset
	;
: lettere
	3 7 penJump
	drW drW drNW drN drN drNE drE drE drSE drSW drW drW drW
	4 -5 penJump
	letterOffset
	;
: letterF
	4 1 penJump
	drW drW drW drW drS drS drS drE drE drW drW drS drS drS
	4 -7 penJump
	letterOffset
	;
: letterf
	3 4 penJump
	drW drW drW
	1 3 penJump
	drN drN drN drN drN drNE drE drSE
	0 -2 penJump
	letterOffset
	;
: letterG
	3 4 penJump
	drE drS drS drS drW drW drW drNW drN drN drN drN drNE drE drE drE
	0 -1 penJump
	letterOffset 
	;
: letterg
	4 7 penJump
	drW drW drW drNW drN drN drNE drE drE drSE drS drS drS drS drSW drW drW drNW
	4 -8 penJump
	letterOffset
	;
: letterH
	0 1 penJump
	drS drS drS drS drS drS drN drN drN drE drE drE drE drS drS drS drN drN drN drN drN drN
	0 -1 penJump
	letterOffset
	;
: letterh
	0 1 penJump
	drS drS drS drS drS drS drN drN drNE drNE drE drSE drS drS drS
	0 -7 penJump
	letterOffset
	;
: letterI
	1 1 penJump
	drE drE drW drS drS drS drS drS drS drW drE drE
	1 -7 penJump
	letterOffset
	;
: letteri
	2 1 penJump
	drS
	0 1 penJump
	drS drS drS drS
	2 -7 penJump
	letterOffset
	;
: letterJ
	4 1 penJump
	drS drS drS drS drS drSW drW drW drNW
	4 -6 penJump
	letterOffset
	;
: letterj
	 1 8 penJump
	drSE drE drNE drN drN drN drN drN
	0 -1 penJump
	drN
	0 -1 penJump
	letterOffset 
	;
: letterK
	0 1 penJump
	drS drS drS drS drS drS drN drN drN drE drNE drNE drNE drSW drSW drSW drSE drSE drSE
	0 -7 penJump
	letterOffset
	;
: letterk
	0 1 penJump
	drS drS drS drS drS drS drN drN drE drE drSE drSE drNW drNW drNE drNE
	0 -3 penJump
	letterOffset
	;
: letterL
	0 1 penJump
	drS drS drS drS drS drS drE drE drE drE
	0 -7 penJump
	letterOffset
	;
: letterl
	2 1 penJump
	drS drS drS drS drS drSE
	1 -7 penJump
	letterOffset
 ;
: letterM
	0 7 penJump
	drN drN drN drN drN drN drSE drSE drNE drNE drS drS drS drS drS drS
	0 -7 penJump
	letterOffset
 ;
: letterm
	0 7 penJump
	drN drN drN drN drS drNE drSE drS drN drNE drSE drS drS drS
	0 -7 penJump
	letterOffset
	;
: letterN
	0 7 penJump
	drN drN drN drN drN drN drS drSE drSE drSE drSE drS drN drN drN drN drN drN
	0 -1 penJump
	letterOffset
	;
: lettern
	0 7 penJump
	drN drN drN drN drS drS drNE drNE drE drSE drS drS drS
	0 -7 penJump
	letterOffset
	;
: letterO
	0 2 penJump
	drS drS drS drS drSE drE drE drNE drN drN drN drN drNW drW drW drSW
	4 -2 penJump
	letterOffset
	;
: lettero
	0 4 penJump
	drS drS drSE drE drE drNE drN drN drNW drW drW drSW
	4 -4 penJump
	letterOffset
	;
: letterP
	0 7 penJump
	drN drN drN drN drN drN drE drE drE drSE drS drSW drW drW drW
	4 -4 penJump
	letterOffset
	;
: letterp
	 0 9 penJump
	drN drN drN drN drN drN drS drNE drE drE drSE drS drS drSW drW drW drW
	4 -7 penJump
	letterOffset
	;
: letterQ
	0 2 penJump
	drS drS drS drS drSE drE drNE drSE drNW drNW drSE drNE drN drN drN drNW drW drW drSW
	4 -2 penJump
	letterOffset
	;
: letterq
	4 9 penJump
	drN drN drN drN drN drN drS drNW drW drW drSW drS drS drSE drE drE drE
	0 -7 penJump
	letterOffset
	;
: letterR
	0 7 penJump
	drN drN drN drN drN drN drE drE drE drSE drS drSW drW drW drW drE drSE drSE drSE
	0 -7 penJump
	letterOffset
	;
: letterr
	0 7 penJump
	drN drN drN drN drS drS drNE drNE drE drSE
	0 -4 penJump
	letterOffset
	;
: letterS
	0 6 penJump
	drSE drE drE drNE drNW drNW drNW drNW drNE drE drE drSE
	0 -2 penJump
	letterOffset
	;
: letters
	0 7 penJump
	drE drE drE drNE drNW drW drW drNW drNE drE drE drE
	0 -3 penJump
	letterOffset
	;
: letterT
	0 1 penJump
	drE drE drE drE drW drW drS drS drS drS drS drS
	2 -7 penJump
	letterOffset
	;
: lettert
	0 3 penJump
	drE drE drE drE drW drW drN drN drS drS drS drS drS drSE drNE
	0 -6 penJump
	letterOffset
	;
: letterU
	0 1 penJump
	drS drS drS drS drS drSE drE drE drNE drN drN drN drN drN
	0 -1 penJump
	letterOffset
	;
: letteru
	0 3 penJump
	drS drS drS drSE drE drNE drNE drN drN drS drS drS drS
	0 -7 penJump
	letterOffset
	;
: letterV
	0 1 penJump
	drS drS drS drS drSE drSE drNE drNE drN drN drN drN
	0 -1 penJump
	letterOffset
 ;
: letterv
	0 3 penJump
	drS drS drSE drSE drNE drNE drN drN
	0 -3 penJump
	letterOffset
	;
: letterW
	0 1 penJump
	drS drS drS drS drS drSE drNE drN drN drS drS drSE drNE drN drN drN drN drN
	0 -1 penJump
	letterOffset
 	;
: letterw
	0 3 penJump
	drS drS drS drSE drNE drN drS drSE drNE drN drN drN
	0 -3 penJump
	letterOffset
 	;
: letterX
	0 1 penJump
	drS drSE drSE drSW drSW drS drN drNE drNE drSE drSE drS drN drNW drNW drNE drNE drN
	0 -1 penJump
	letterOffset
	;
: letterx
	0 3 penJump
	drSE drSE drSW drSW drNE drNE drSE drSE drNW drNW drNE drNE
	0 -3 penJump
	letterOffset
 ;
: letterY
	0 1 penJump
	drS drSE drSE drS drS drS drN drN drN drNE drNE drN
	0 -1 penJump
	letterOffset
	;
: lettery
	0 3 penJump
	drS drS drSE drSE drE drNE drN drN drN drS drS drS drSW drSW drSW drW
	4 -9 penJump
	letterOffset
 ;
: letterZ
	0 1 penJump
	drE drE drE drE drS drSW drSW drSW drSW drS drE drE drE drE
	0 -7 penJump
	letterOffset
 ;
: letterz
	0 3 penJump
	drE drE drE drE drSW drSW drSW drSW drE drE drE drE
	0 -7 penJump
	letterOffset
	;
: letter{
	3 1 penJump
	drSW drS drSE drW drW drE drE drSW drS drSE
	1 -7 penJump
	letterOffset
	;
: letter| 
	2 1 penJump
	drS drS drS drS drS drS
	2 -7 penJump
	letterOffset
	;
: letter}
	1 1 penJump
	drSE drS drSW drE drE drW drW drSE drS drSW
	3 -7 penJump
	letterOffset
	;
: letter!
	2 1 penJump
	drS drS drS drS
	0 1 penJump
	drS
	2 -7 penJump
	letterOffset
	;
: letter"
	2 1 penJump
	drSW
	2 -1 penJump
	drSW
	2 -2 penJump
	letterOffset
	;
: letter#
	2 3 penJump
	drS drS drS
	1 0 penJump
	drN drN drN
	1 1 penJump
	drW drW drW
	0 1 penJump
	drE drE drE
	0 -5 penJump
	letterOffset
	;
: letter$
	4 3 penJump
	drNW drW drN drS drW drSW drSE drE drE drSE drSW drW drS drN drW drNW
	4 -5 penJump
	letterOffset
	;
: letter%
	0 4 penJump
	drNE drE drSW drW
	0 3 penJump
	drNE drNE drNE drNE
	0 3 penJump
	drSW drW drNE drE
	0 -6 penJump
	letterOffset
	;
: letter&
	4 7 penJump
	drNW drNW drNW drN drE drS drSW drSW drSE drE drNE drNE
	0 -5 penJump
	letterOffset
	;
: letter''
	3 1 penJump
	drSW
	2 -2 penJump
	letterOffset
	;

: letter(
	3 1 penJump
	drSW drS drS drS drS drSE
	1 -7 penJump
	letterOffset
	;
: letter)
	1 1 penJump
	drSE drS drS drS drS drSW
	3 -7 penJump
	letterOffset
	;
: letter*
	1 3 penJump
	drSE drN drS drNE drSW drE drW drSE drNW drS drN drSW drNE drW
	3 -4 penJump
	letterOffset
	;
: letter+
	2 3 penJump
	drS drS drN drW drE drE
	1 -4 penJump
	letterOffset
	;
: letter,
	2 6 penJump
	drS drSW
	3 -8 penJump
	letterOffset
	;
: letter-
	1 4 penJump
	drE drE
	1 -4 penJump
	letterOffset
	;
: letter.
	2 6 penJump
	drS
	2 -7 penJump
	letterOffset
	;
: letter/
	0 6 penJump
	drNE drNE drNE drNE
	0 -2 penJump
	letterOffset
	;
: letter:
2 4 penJump
drS
0 1 penJump
drS
2 -7 penJump
letterOffset
	;
: letter;
2 4 penJump
drS
0 1 penJump
drS drSW
3 -8 penJump
letterOffset
	;
: letter<
	3 2 penJump
	drSW drSW drSE drSE
	1 -6 penJump
	letterOffset
	;
: letter=
	1 3 penJump
	drE drE drE
	-3 2 penJump
	drE drE drE
	0 -5 penJump
	letterOffset
	;
: letter>
	1 2 penJump
	drSE drSE drSW drSW
	3 -6 penJump
	letterOffset
	;
: letter?
	0 2 penJump
	drNE drE drE drSE drSW drW drW drSW drSE drE drE drNE
	-2 2 penJump
	drS
	2 -7 penJump
	letterOffset
	;
: letter@
	3 5 penJump
	drW drW drN drNE drE drS drS drE drN drN drNW drW drW drSW drS drS drSE drE drE
	1 -6 penJump
	letterOffset
	;
: letter[
	3 1 penJump
	drW drW drS drS drS drS drS drS drE drE
	1 -7 penJump
	letterOffset
	;
: letter\
	0 2 penJump
	drSE drSE drSE drSE
	0 -6 penJump
	letterOffset
	;
: letter]
	1 1 penJump
	drE drE drS drS drS drS drS drS drW drW
	3 -7 penJump
	letterOffset
	;
: letter^
	1 2 penJump
	drNE drSE
	1 -2 penJump
	letterOffset
	;
: letter_
	0 7 penJump
	drE drE drE drE
	0 -7 penJump
	letterOffset
	;
: letter`
	1 1 penJump
	drSE
	2 -2 penJump
	letterOffset
	;
: letter~
	0 5 penJump
	drNE drE drSE drNE
	0 -4 penJump
	letterOffset
	;
: letterNone ;
: letterSpace 
	4 0 penJump
	letterOffset
	;

: characterTable 
	letterNone  (   0 )
	letterNone  (   1 )
	letterNone  (   2 )
	letterNone  (   3 )
	letterNone  (   4 )
	letterNone  (   5 )
	letterNone  (   6 )
	letterNone  (   7 )
	letterNone  (   8 )
	letterNone  (   9 )
	letterNone  (  10 )
	letterNone  (  11 )
	letterNone  (  12 )
	letterNone  (  13 )
	letterNone  (  14 )
	letterNone  (  15 )
	letterNone  (  16 )
	letterNone  (  17 )
	letterNone  (  18 )
	letterNone  (  19 )
	letterNone  (  20 )
	letterNone  (  21 )
	letterNone  (  22 )
	letterNone  (  23 )
	letterNone  (  24 )
	letterNone  (  25 )
	letterNone  (  26 )
	letterNone  (  27 )
	letterNone  (  28 )
	letterNone  (  29 )
	letterNone  (  30 )
	letterNone  (  31 )
	letterSpace (  32 )
	letter!     (  33 )
	letter"     (  34 )
	letter#     (  35 )
	letter$     (  36 )
	letter%     (  37 )
	letter&     (  38 )
	letter''    (  39 )
	letter(     (  40 )
	letter)     (  41 )
	letter*     (  42 )
	letter+     (  43 )
	letter,     (  44 )
	letter-     (  45 )
	letter.     (  46 )
	letter/     (  47 )
	letter0     (  48 )
	letter1     (  49 )
	letter2     (  50 )
	letter3     (  51 )
	letter4     (  52 )
	letter5     (  53 )
	letter6     (  54 )
	letter7     (  55 )
	letter8     (  56 )
	letter9     (  57 )
	letter:     (  58 )
	letter;     (  59 )
	letter<     (  60 )
	letter=     (  61 )
	letter>     (  62 )
	letter?     (  63 )
	letter@     (  64 )
	letterA     (  65 )
	letterB     (  66 )
	letterC     (  67 )
	letterD     (  68 )
	letterE     (  69 )
	letterF     (  70 )
	letterG     (  71 )
	letterH     (  72 )
	letterI     (  73 )
	letterJ     (  74 )
	letterK     (  75 )
	letterL     (  76 )
	letterM     (  77 )
	letterN     (  78 )
	letterO     (  79 )
	letterP     (  80 )
	letterQ     (  81 )
	letterR     (  82 )
	letterS     (  83 )
	letterT     (  84 )
	letterU     (  85 )
	letterV     (  86 )
	letterW     (  87 )
	letterX     (  88 )
	letterY     (  89 )
	letterZ     (  90 )
	letter[     (  91 )
	letter\     (  92 )
	letter]     (  93 )
	letter^     (  94 )
	letter_     (  95 )
	letter`     (  96 )
	lettera     (  97 )
	letterb     (  98 )
	letterc     (  99 )
	letterd     ( 100 )
	lettere     ( 101 )
	letterf     ( 102 )
	letterg     ( 103 )
	letterh     ( 104 )
	letteri     ( 105 )
	letterj     ( 106 )
	letterk     ( 107 )
	letterl     ( 108 )
	letterm     ( 109 )
	lettern     ( 110 )
	lettero     ( 111 )
	letterp     ( 112 )
	letterq     ( 113 )
	letterr     ( 114 )
	letters     ( 115 )
	lettert     ( 116 )
	letteru     ( 117 )
	letterv     ( 118 )
	letterw     ( 119 )
	letterx     ( 120 )
	lettery     ( 121 )
	letterz     ( 122 )
	letter{     ( 123 )
	letter|     ( 124 )
	letter}		( 125 )
	letter~     	( 126 )
	;
	
: printAscii  4 * '' characterTable 4+ + @ EXECUTE ;

: emit DUP 127 > IF letterNone ELSE printAscii THEN ;

: printAll 32 BEGIN DUP 127 <= WHILE DUP DUP emit 1+ REPEAT   ;

printAll
'.


forth run.

forth memory  stackAsUnsignedArray 
]

{ #category : #accessing }
Forth class >> plotLetters [

<script>
| forth |	
	
forth := ForthUppercase new init.
forth openPlotter.
forth loadStd.

forth << '

: variable: VARIABLE LATEST @ >CFA EXECUTE ! ;
: ->cell 4 SWAP +! ;
: <-cell 4 SWAP -! ;

32 CELLS ALLOT variable: loopSP
loopSP @ CONSTANT loopTop
: >loop loopSP @ ! loopSP ->cell ;
: loop> loopSP <-cell loopSP @ @ ; 
: do IMMEDIATE '' >loop , '' >loop , [COMPILE] BEGIN ;
: loopCheck loop> loop> 1+  2DUP =  -ROT >loop >loop ;
: loopFinish loop> DROP loop> DROP ;
: loop IMMEDIATE '' loopCheck , [COMPILE] UNTIL '' loopFinish , ;

: mm 10 * ;
: cm 100 * ;
: dot ;
: dots ;
: pt 4 dots * ;

: penTranslate BEGIN DUP 0> WHILE PENSTEP 1- REPEAT DROP ;
: penMove PENUP penTranslate ;
: penDraw PENDOWN penTranslate ;

: abs DUP 0< IF DUP DUP + - THEN ;
 
: penJump 
   PENRESET 
   DUP 0 >= IF PENSOUTH ELSE PENNORTH THEN
   abs penMove PENRESET 
   OVER 0 >= IF PENRIGHT ELSE PENLEFT THEN
   abs penMove PENRESET ;

: drawN PENRESET PENNORTH 1 pt penDraw ;
: drawS PENRESET PENSOUTH 1 pt penDraw ;
: drawE PENRESET PENRIGHT 1 pt penDraw ;
: drawW PENRESET PENLEFT 1 pt penDraw ;
: drawNE PENRESET PENNORTH PENRIGHT 1 pt penDraw ;
: drawNW PENRESET PENNORTH PENLEFT 1 pt penDraw ;
: drawSE PENRESET PENSOUTH PENRIGHT 1 pt penDraw ;
: drawSW PENRESET PENSOUTH PENLEFT 1 pt penDraw ;

: moveN PENRESET PENNORTH 1 pt penMove ;
: moveS PENRESET PENSOUTH 1 pt penMove ;
: moveE PENRESET PENRIGHT 1 pt penMove ;
: moveW PENRESET PENLEFT 1 pt penMove ;
: moveNE PENRESET PENNORTH PENRIGHT 1 pt penMove ;
: moveNW PENRESET PENNORTH PENLEFT 1 pt penMove ;
: moveSE PENRESET PENSOUTH PENRIGHT 1 pt penMove ;
: moveSW PENRESET PENSOUTH PENLEFT 1 pt penMove ;

: letterOffset moveE ;
	
: letterA
	 0 pt 7 pt penJump
	drawN drawN drawN drawN drawN drawNE drawE drawE drawSE drawS drawS drawS drawS drawS drawN drawN drawN drawW drawW drawW drawW
	4 pt -4 pt penJump
	letterOffset ;
: lettera
	 3 pt 6 pt penJump
	drawSW drawW drawNW drawN drawN drawNE drawE drawSE drawS drawS drawSE
	0 pt -7 pt penJump
	letterOffset ;		
: letterB
	0 pt 1 pt penJump
	drawE drawE drawE drawSE drawS drawSW drawW drawW drawE drawE drawSE drawS drawSW drawW drawW drawW drawE drawN drawN drawN drawN drawN drawN
	3 pt -1 pt penJump
	letterOffset ;
: letterb
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawS drawN drawN drawSE drawSE drawE drawNE drawN drawN drawNW drawW drawSW drawSW
	4 pt -5 pt penJump
	letterOffset ;
: letterC
	4 pt 2 pt penJump
	drawNW drawW drawW drawSW drawS drawS drawS drawS drawSE drawE drawE drawNE
	0 pt -6 pt penJump
	letterOffset ;
: letterc
	4 pt 7 pt penJump
	drawW drawW drawW drawNW drawN drawN drawNE drawE drawE drawE
	0 pt -3 pt penJump
	letterOffset ;
: letterD
	0 pt 1 pt penJump
	drawE drawE drawE drawSE drawS drawS drawS drawS drawSW drawW drawW drawW drawE drawN drawN drawN drawN drawN drawN
	3 pt -1 pt penJump
	letterOffset ;
: letterd
	4 pt 5 pt penJump
	drawNW drawNW drawW drawSW drawS drawS drawSE drawE drawNE drawNE drawS drawS drawN drawN drawN drawN drawN drawN
	0 pt -1 pt penJump
	letterOffset ;
: letterE
	4 pt 1 pt penJump
	drawW drawW drawW drawW drawS drawS drawS drawE drawE drawW drawW drawS drawS drawS drawE drawE drawE drawE
	0 pt -7 pt penJump
letterOffset ;
: lettere
	3 pt 7 pt penJump
	drawW drawW drawNW drawN drawN drawNE drawE drawE drawSE drawSW drawW drawW drawW
	4 pt -5 pt penJump
	letterOffset ;
: letterF
	4 pt 1 pt penJump
	drawW drawW drawW drawW drawS drawS drawS drawE drawE drawW drawW drawS drawS drawS
	4 pt -7 pt penJump
	letterOffset ;
: letterf
	3 pt 4 pt penJump
	drawW drawW drawW
	1 pt 3 pt penJump
	drawN drawN drawN drawN drawN drawNE drawE drawSE
	0 pt -2 pt penJump
	letterOffset ;
: letterG
	3 pt 4 pt penJump
	drawE drawS drawS drawS drawW drawW drawW drawNW drawN drawN drawN drawN drawNE drawE drawE drawE
	0 pt -1 pt penJump
	letterOffset ;
: letterg
	4 pt 7 pt penJump
	drawW drawW drawW drawNW drawN drawN drawNE drawE drawE drawSE drawS drawS drawS drawS drawSW drawW drawW drawNW
	4 pt -8 pt penJump
	letterOffset ;
: letterH
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawS drawN drawN drawN drawE drawE drawE drawE drawS drawS drawS drawN drawN drawN drawN drawN drawN
	0 pt -1 pt penJump
	letterOffset ;
: letterh
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawS drawN drawN drawNE drawNE drawE drawSE drawS drawS drawS
	0 pt -7 pt penJump
	letterOffset ;
: letterI
	1 pt 1 pt penJump
	drawE drawE drawW drawS drawS drawS drawS drawS drawS drawW drawE drawE
	1 pt -7 pt penJump
	letterOffset ;
: letteri
	2 pt 1 pt penJump
	drawS
	0 pt 1 pt penJump
	drawS drawS drawS drawS
	2 pt -7 pt penJump
	letterOffset ;
: letterJ
	4 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawSW drawW drawW drawNW
	4 pt -6 pt penJump
	letterOffset ;
: letterj
	 1 pt 8 pt penJump
	drawSE drawE drawNE drawN drawN drawN drawN drawN
	0 pt -1 pt penJump
	drawN
	0 pt -1 pt penJump
	letterOffset ;
: letterK
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawS drawN drawN drawN drawE drawNE drawNE drawNE drawSW drawSW drawSW drawSE drawSE drawSE
	0 pt -7 pt penJump
	letterOffset ;
: letterk
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawS drawN drawN drawE drawE drawSE drawSE drawNW drawNW drawNE drawNE
	0 pt -3 pt penJump
	letterOffset ;
: letterL
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawS drawE drawE drawE drawE
	0 pt -7 pt penJump
	letterOffset ;
: letterl
	2 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawSE
	1 pt -7 pt penJump
	letterOffset  ;
: letterM
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawN drawN drawSE drawSE drawNE drawNE drawS drawS drawS drawS drawS drawS
	0 pt -7 pt penJump
	letterOffset ;
: letterm
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawS drawNE drawSE drawS drawN drawNE drawSE drawS drawS drawS
	0 pt -7 pt penJump
	letterOffset ;
: letterN
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawN drawN drawS drawSE drawSE drawSE drawSE drawS drawN drawN drawN drawN drawN drawN
	0 pt -1 pt penJump
	letterOffset ;
: lettern
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawS drawS drawNE drawNE drawE drawSE drawS drawS drawS
	0 pt -7 pt penJump
	letterOffset ;
: letterO
	0 pt 2 pt penJump
	drawS drawS drawS drawS drawSE drawE drawE drawNE drawN drawN drawN drawN drawNW drawW drawW drawSW
	4 pt -2 pt penJump
	letterOffset ;
: lettero
	0 pt 4 pt penJump
	drawS drawS drawSE drawE drawE drawNE drawN drawN drawNW drawW drawW drawSW
	4 pt -4 pt penJump
	letterOffset ;
: letterP
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawN drawN drawE drawE drawE drawSE drawS drawSW drawW drawW drawW
	4 pt -4 pt penJump
	letterOffset ;
: letterp
	 0 pt 9 pt penJump
	drawN drawN drawN drawN drawN drawN drawS drawNE drawE drawE drawSE drawS drawS drawSW drawW drawW drawW
	4 pt -7 pt penJump
	letterOffset ;
: letterQ
	0 pt 2 pt penJump
	drawS drawS drawS drawS drawSE drawE drawNE drawSE drawNW drawNW drawSE drawNE drawN drawN drawN drawNW drawW drawW drawSW
	4 pt -2 pt penJump
	letterOffset ;
: letterq
	4 pt 9 pt penJump
	drawN drawN drawN drawN drawN drawN drawS drawNW drawW drawW drawSW drawS drawS drawSE drawE drawE drawE
	0 pt -7 pt penJump
	letterOffset ;
: letterR
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawN drawN drawE drawE drawE drawSE drawS drawSW drawW drawW drawW drawE drawSE drawSE drawSE
	0 pt -7 pt penJump
	letterOffset  ;
: letterr
	0 pt 7 pt penJump
	drawN drawN drawN drawN drawS drawS drawNE drawNE drawE drawSE
	0 pt -4 pt penJump
	letterOffset ;
: letterS
	0 pt 6 pt penJump
	drawSE drawE drawE drawNE drawNW drawNW drawNW drawNW drawNE drawE drawE drawSE
	0 pt -2 pt penJump
	letterOffset ;
: letters
	0 pt 7 pt penJump
	drawE drawE drawE drawNE drawNW drawW drawW drawNW drawNE drawE drawE drawE
	0 pt -3 pt penJump
	letterOffset ;
: letterT
	0 pt 1 pt penJump
	drawE drawE drawE drawE drawW drawW drawS drawS drawS drawS drawS drawS
	2 pt -7 pt penJump
	letterOffset ;
: lettert
	0 pt 3 pt penJump
	drawE drawE drawE drawE drawW drawW drawN drawN drawS drawS drawS drawS drawS drawSE drawNE
	0 pt -6 pt penJump
	letterOffset ;
: letterU
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawSE drawE drawE drawNE drawN drawN drawN drawN drawN
	0 pt -1 pt penJump
letterOffset ;
: letteru
	0 pt 3 pt penJump
	drawS drawS drawS drawSE drawE drawNE drawNE drawN drawN drawS drawS drawS drawS
	0 pt -7 pt penJump
	letterOffset ;
: letterV
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawSE drawSE drawNE drawNE drawN drawN drawN drawN
	0 pt -1 pt penJump
	letterOffset ;
: letterv
	0 pt 3 pt penJump
	drawS drawS drawSE drawSE drawNE drawNE drawN drawN
	0 pt -3 pt penJump
	letterOffset ;
: letterW
	0 pt 1 pt penJump
	drawS drawS drawS drawS drawS drawSE drawNE drawN drawN drawS drawS drawSE drawNE drawN drawN drawN drawN drawN
	0 pt -1 pt penJump
	letterOffset ;
: letterw
	0 pt 3 pt penJump
	drawS drawS drawS drawSE drawNE drawN drawS drawSE drawNE drawN drawN drawN
	0 pt -3 pt penJump
	letterOffset ;
: letterX
	0 pt 1 pt penJump
	drawS drawSE drawSE drawSW drawSW drawS drawN drawNE drawNE drawSE drawSE drawS drawN drawNW drawNW drawNE drawNE drawN
	0 pt -1 pt penJump
	letterOffset ;
: letterx
	0 pt 3 pt penJump
	drawSE drawSE drawSW drawSW drawNE drawNE drawSE drawSE drawNW drawNW drawNE drawNE
	0 pt -3 pt penJump
	letterOffset ;
: letterY
	0 pt 1 pt penJump
	drawS drawSE drawSE drawS drawS drawS drawN drawN drawN drawNE drawNE drawN
	0 pt -1 pt penJump
	letterOffset ;
: lettery
	0 pt 3 pt penJump
	drawS drawS drawSE drawSE drawE drawNE drawN drawN drawN drawS drawS drawS drawSW drawSW drawSW drawW
	4 pt -9 pt penJump
	letterOffset ;
: letterZ
	0 pt 1 pt penJump
	drawE drawE drawE drawE drawS drawSW drawSW drawSW drawSW drawS drawE drawE drawE drawE
	0 pt -7 pt penJump
	letterOffset ;
: letterz
	0 pt 3 pt penJump
	drawE drawE drawE drawE drawSW drawSW drawSW drawSW drawE drawE drawE drawE
	0 pt -7 pt penJump
	letterOffset ;

letterF lettero letterr lettert letterh
	
\ letterA lettera letterB letterb letterC letterc letterD letterd letterE 
\ lettere letterF letterf letterG letterg letterH letterh letterI letteri 
\ letterJ letterj letterK letterk letterL letterl letterM letterm letterN 
\ lettern letterO lettero letterP letterp letterQ letterq letterR letterr 
\ letterS letters letterT lettert letterU letteru letterV letterv letterW 
\ letterw letterX letterx letterY lettery letterZ letterz
'.

forth run.




]

{ #category : #accessing }
Forth class >> plotNumbers [

<script>
| forth |	
	
forth := ForthUppercase new init.
forth openPlotter.
forth loadStd.

forth << '

: variable: VARIABLE LATEST @ >CFA EXECUTE ! ;
: ->cell 4 SWAP +! ;
: <-cell 4 SWAP -! ;

32 CELLS ALLOT variable: loopSP
loopSP @ CONSTANT loopTop
: >loop loopSP @ ! loopSP ->cell ;
: loop> loopSP <-cell loopSP @ @ ; 
: do IMMEDIATE '' >loop , '' >loop , [COMPILE] BEGIN ;
: loopCheck loop> loop> 1+  2DUP =  -ROT >loop >loop ;
: loopFinish loop> DROP loop> DROP ;
: loop IMMEDIATE '' loopCheck , [COMPILE] UNTIL '' loopFinish , ;

: mm 10 * ;
: cm 100 * ;
: dot ;
: dots ;
: pt 8 dots * ;

: penTranslate BEGIN DUP 0> WHILE PENSTEP 1- REPEAT DROP ;
: penMove PENUP penTranslate ;
: penDraw PENDOWN penTranslate ;

: abs DUP 0< IF DUP DUP + - THEN ;
 
: penJump 
   PENRESET 
   DUP 0 >= IF PENSOUTH ELSE PENNORTH THEN
   abs penMove PENRESET 
   OVER 0 >= IF PENRIGHT ELSE PENLEFT THEN
   abs penMove PENRESET ;

: drawN PENRESET PENNORTH 1 pt penDraw ;
: drawS PENRESET PENSOUTH 1 pt penDraw ;
: drawE PENRESET PENRIGHT 1 pt penDraw ;
: drawW PENRESET PENLEFT 1 pt penDraw ;
: drawNE PENRESET PENNORTH PENRIGHT 1 pt penDraw ;
: drawNW PENRESET PENNORTH PENLEFT 1 pt penDraw ;
: drawSE PENRESET PENSOUTH PENRIGHT 1 pt penDraw ;
: drawSW PENRESET PENSOUTH PENLEFT 1 pt penDraw ;

: moveN PENRESET PENNORTH 1 pt penMove ;
: moveS PENRESET PENSOUTH 1 pt penMove ;
: moveE PENRESET PENRIGHT 1 pt penMove ;
: moveW PENRESET PENLEFT 1 pt penMove ;
: moveNE PENRESET PENNORTH PENRIGHT 1 pt penMove ;
: moveNW PENRESET PENNORTH PENLEFT 1 pt penMove ;
: moveSE PENRESET PENSOUTH PENRIGHT 1 pt penMove ;
: moveSW PENRESET PENSOUTH PENLEFT 1 pt penMove ;

: letterOffset moveE ;

: letter1 
	1 pt 2 pt penJump
	drawNE drawS drawS drawS drawS drawS drawS drawW drawE drawE
	1 pt -7 pt penJump
	letterOffset ;
: letter2
	0 pt 2 pt penJump
	drawNE drawE drawE drawSE drawS drawSW drawW drawW drawSW drawS drawS drawE drawE drawE drawE
	0 pt -7 pt penJump
	letterOffset ;
: letter3
	0 pt 2 pt penJump
	drawNE drawE drawE drawSE drawS drawSW drawW drawE drawSE drawS drawSW drawW drawW drawNW
	4 pt -6 pt penJump
letterOffset ;
: letter4
	4 pt 5 pt penJump
	drawW drawW drawW drawW drawN drawNE drawNE drawNE drawS drawS drawS drawS drawS drawS
	1 pt -7 pt penJump
	letterOffset ;
: letter5
	4 pt 1 pt penJump
	drawW drawW drawW drawW drawS drawS drawE drawE drawE drawSE drawS drawS drawSW drawW drawW drawNW
	4 pt -6 pt penJump
	letterOffset ;
: letter6
	3 pt 1 pt penJump
	drawW drawSW drawSW drawS drawS drawS drawSE drawE drawE drawNE drawN drawNW drawW drawW drawW
	4 pt -4 pt penJump
	letterOffset ;
: letter7
	0 pt 1 pt penJump
	drawE drawE drawE drawE drawSW drawSW drawSW drawS drawS drawS
	3 pt -7 pt penJump
	letterOffset ;		
: letter8
	0 pt 2 pt penJump
	drawS drawSE drawE drawE drawSE drawS drawSW drawW drawW drawNW drawN drawNE drawE drawE drawNE drawN drawNW drawW drawW drawSW
	4 pt -2 pt penJump
	letterOffset ;	
: letter9
	4 pt 4 pt penJump
	drawW drawW drawW drawNW drawN drawNE drawE drawE drawSE drawS drawS drawS drawSW drawSW drawW
	3 pt -7 pt penJump
	letterOffset ;
	
: letter0
	0 pt 6 pt penJump
	drawSE drawE drawE drawNE drawN drawN drawN drawN drawNW drawW drawW drawSW drawS drawS drawS drawS drawNE drawNE drawNE drawNE
	0 pt -2 pt penJump
	letterOffset ;	
					
letter1 letter2 letter3 letter4 letter5 letter6 letter7 letter8 letter9 letter0

'.

forth run.


]

{ #category : #streaming }
Forth >> << aString [

	self inputBuffer addAll: (aString collect: #asciiValue as: Array).
]

{ #category : #accessing }
Forth >> addLabel: aLabel address: anAddress [

	^ labels at: aLabel put: anAddress

]

{ #category : #adding }
Forth >> addToKeyboardBuffer: aCharacter [

	| wasRunning |
	wasRunning := state = #running.
	keyboardBuffer add: aCharacter asciiValue.
	wasRunning ifFalse: [ state := #running. self run ]
]

{ #category : #accessing }
Forth >> addressForLabel: aLabel [

	^ labels at: aLabel
]

{ #category : #accessing }
Forth >> addressForLabel: aLabel inFutureSet: address [

	^ labels at: aLabel ifAbsent: [ 
		self registerUnknownLabel: aLabel usageAtPosition: address.
		0 ]
]

{ #category : #memory }
Forth >> byteAt: address [ 

	^ memory byteAt: address 
]

{ #category : #'as yet unclassified' }
Forth >> codewordOf: dictionaryWordAddress [

	| current length | 
	current := dictionaryWordAddress + self wordSize.
	length := self lengthByteAt: current.
	
	^ current + 1 + length
]

{ #category : #memory }
Forth >> detectCode: aCodeClass [

	^ memory detectCode: aCodeClass
]

{ #category : #initialization }
Forth >> dsp [

	^ self memory dsp
]

{ #category : #accessing }
Forth >> find: nameArray [

	| current found |

	current := self varLatestValue.
	
	[  current = 0 ifTrue: [ ^ 0 ].
		found := (self match: nameArray at: current) and: [(self isHidden: current) not].
		found ifFalse: [ current := self wordAt: current ].
		found
	] whileFalse.

	^ current
	
	
	
]

{ #category : #initialization }
Forth >> fixUnknownLabels [

	unknownLabels keysAndValuesDo: [ :label :usages |
		| correctAddress |
		correctAddress := self addressForLabel: label.
		usages do: [ :usageAddress | self writeWord: correctAddress at: usageAddress ] ].
	
	unknownLabels := Dictionary new.
]

{ #category : #accessing }
Forth >> flagHidden [

	^ 16r20
]

{ #category : #accessing }
Forth >> flagImmediate [

	^ 16r80
]

{ #category : #accessing }
Forth >> flagLengthMask [

	^ 16r1F
]

{ #category : #initialization }
Forth >> init [ 

	self pc: 1. "docol" 
	self pcCurrent: (self addressForLabel: 'codeword_QUIT').
	self pcNext: self pcCurrent + self wordSize.
	self varStateValue: 0.

]

{ #category : #initialization }
Forth >> initPos [

	^ initPos
]

{ #category : #accessing }
Forth >> initPos: anObject [
	initPos := anObject
]

{ #category : #initialization }
Forth >> initialize [

	memory := ForthMemory new forth: self.
	labels := Dictionary new.
	unknownLabels := Dictionary new.
	lastWord := 0.
	
	inputBuffer := OrderedCollection new.
	outputBuffer := OrderedCollection new.
	keyboardBuffer := OrderedCollection new.
		
	self memoryInitializer initializeMemory.
	
	state := #running.
]

{ #category : #initialization }
Forth >> inputBuffer [

	^ inputBuffer 
]

{ #category : #'as yet unclassified' }
Forth >> isHidden: dictionaryWordAddress [

	^ ((self byteAt: (dictionaryWordAddress + self wordSize)) bitAnd: self flagHidden) ~= 0. 
]

{ #category : #'as yet unclassified' }
Forth >> isImmediate: dictionaryWordAddress [

	^ ((self byteAt: (dictionaryWordAddress + self wordSize)) bitAnd: self flagImmediate) ~= 0. 
]

{ #category : #accessing }
Forth >> labels [ 

	^ labels
]

{ #category : #accessing }
Forth >> labels: anObject [
	labels := anObject
]

{ #category : #'as yet unclassified' }
Forth >> labelsFor: index [ 

	^ labels associations select: [ :assoc |
		assoc value = index ] 
]

{ #category : #accessing }
Forth >> lastWord [
	^ lastWord
]

{ #category : #accessing }
Forth >> lastWord: anObject [
	lastWord := anObject
]

{ #category : #initialization }
Forth >> lengthByteAt: address [ 

	^ (self memoryAt: address) bitAnd: self lengthMask
	

]

{ #category : #accessing }
Forth >> lengthMask [

	^ 16r1F
]

{ #category : #'as yet unclassified' }
Forth >> loadStd [

	self << (self class package mcWorkingCopy ancestry ancestors first package repository location / 'src-forth' / 'std.f') readStream upToEnd.
]

{ #category : #'as yet unclassified' }
Forth >> loadStdWithPlotter [

	self << (self class package mcWorkingCopy ancestry ancestors first package repository location / 'src-forth' / 'std-plotter.f') readStream upToEnd.
]

{ #category : #accessing }
Forth >> match: nameArray at: address [

	| length anArray |
	length := self lengthByteAt: address + self wordSize.
	length = nameArray size ifFalse: [ ^ false ].
	anArray := self memoryCopyFrom: address + self wordSize + 1 to: address + self wordSize + nameArray size.
	^ nameArray = anArray
	
	
	
	
	
]

{ #category : #accessing }
Forth >> memory [ 

	^ memory
]

{ #category : #memory }
Forth >> memoryAt: address [

	^ memory memoryAt: address

]

{ #category : #memory }
Forth >> memoryAt: address put: anObject [

	^ memory memoryAt: address put: anObject
]

{ #category : #memory }
Forth >> memoryCopyFrom: start to: stop [ 

	^ memory memoryCopyFrom: start to: stop 
]

{ #category : #initialization }
Forth >> memoryInitializer [

	^ ForthStandardMemoryInitializer for: self
]

{ #category : #memory }
Forth >> memoryReset [

	^ memory memoryReset
]

{ #category : #memory }
Forth >> memorySize [

	^ memory memorySize
]

{ #category : #initialization }
Forth >> noInput [

	state := #noInput
]

{ #category : #initialization }
Forth >> outputBuffer [

	^ outputBuffer 
]

{ #category : #'as yet unclassified' }
Forth >> outputBufferString [

	^ String withAll: self outputBuffer asByteArray
]

{ #category : #accessing }
Forth >> pc [
	^ pc
]

{ #category : #accessing }
Forth >> pc: anObject [
	pc := anObject
]

{ #category : #accessing }
Forth >> pcCurrent [
	^ pcCurrent
]

{ #category : #accessing }
Forth >> pcCurrent: anObject [
	pcCurrent := anObject
]

{ #category : #accessing }
Forth >> pcNext [
	^ pcNext
]

{ #category : #accessing }
Forth >> pcNext: anObject [
	pcNext := anObject
]

{ #category : #initialization }
Forth >> peek [

	^ memory peek
]

{ #category : #initialization }
Forth >> peekReturnStack [

	^ memory peekReturnStack
]

{ #category : #initialization }
Forth >> pop [

	^ memory pop
]

{ #category : #initialization }
Forth >> pop: count [

	^ memory pop: count
]

{ #category : #initialization }
Forth >> popFromReturnStack [

	^ memory popFromReturnStack
]

{ #category : #'as yet unclassified' }
Forth >> privComma: value [

	self writeWord: value at: self varHereValue.
	self varHereValue: self varHereValue + self wordSize.
]

{ #category : #initialization }
Forth >> privNext [
	
	self pcCurrent: (self wordAt: self pcNext).
	self pcNext: self pcNext + self wordSize.
	self pc: (self wordAt: self pcCurrent) - 1.
	"subtract one because the address will be immediately increased"
]

{ #category : #'as yet unclassified' }
Forth >> privNumber: wordStringAddress length: length [

	| aString base number allowed |
	
	base := self signedWordAt: (self addressForLabel: 'var_BASE').

	allowed := base <= 10 
		ifTrue: [ #(45), (48 to: 48-1+base) ]
		ifFalse: [ #(45), (48 to: 57), (65 to: 65+base-11), (97 to: 97+base-11) ].

	aString := String withAll: ((self  memoryCopyFrom: wordStringAddress to: wordStringAddress + (length-1)) 
		withIndexCollect: [ :each :index | 		
			(allowed includes: each)
				ifFalse: [ ^ number -> index ].
			Character value: each ]).
	
	number := Number readFrom: aString readStream base: base.
	
	^ number -> 0 "no error"
	
]

{ #category : #accessing }
Forth >> privReadKeyboard [

	keyboardBuffer isEmpty ifTrue: [ 
		self noInput.
		^ 0 ].

	^ keyboardBuffer removeFirst.
]

{ #category : #accessing }
Forth >> privWord [

	| length character characterCode|
	
	length := 0.
	
	[ self inputBuffer isEmpty ifTrue: [ 
			self noInput.
			^ self wordBufferAddress -> 0 ].

		characterCode := self inputBuffer removeFirst.
		character := Character value: characterCode.
		character isSeparator.
	] whileTrue.

	[  | result |
		self memoryAt: (self wordBufferAddress + length) put: characterCode.
		length := length + 1.
		result := self inputBuffer isEmpty 
			ifFalse: [ 
				characterCode := self inputBuffer removeFirst.
				character := Character value: characterCode.
				character isSeparator ] 
			ifTrue: [ true ].
	] whileFalse.

	(length = 1 and: [ (self byteAt: self wordBufferAddress) = ($\ asciiValue) ]) ifTrue: [ 
		[ 
		self inputBuffer isEmpty ifTrue: [ 
			self noInput.
			^ self wordBufferAddress -> 0 ].
		characterCode := self inputBuffer removeFirst.
		character := Character value: characterCode.
		(character = Character cr) or: [ character = Character lf ] ] whileFalse.
		^ self privWord ].

	^ self wordBufferAddress -> length
	
	
	
]

{ #category : #initialization }
Forth >> push: bytes [

	^ memory push: bytes
]

{ #category : #initialization }
Forth >> pushToReturnStack: address [ 

	^ memory pushToReturnStack: address 
]

{ #category : #accessing }
Forth >> registerUnknownLabel: aLabel usageAtPosition: address [

	(unknownLabels at: aLabel ifAbsentPut: [ Set new ]) add: address
]

{ #category : #initialization }
Forth >> resetStack [

	memory resetStack
]

{ #category : #initialization }
Forth >> returnStack [ 

	^ memory returnStack
]

{ #category : #initialization }
Forth >> returnStackPush: anArray [

	^ memory returnStackPush: anArray
]

{ #category : #initialization }
Forth >> returnStackSize [

	^ memory returnStackSize
]

{ #category : #initialization }
Forth >> run [ 

	[ state = #running ] whileTrue: [ self step ]
]

{ #category : #'as yet unclassified' }
Forth >> s0 [

	^ memory s0
]

{ #category : #initialization }
Forth >> signedWordAt: address [ 

	| unsigned |
	
	unsigned := ((self memoryAt: address + 3)) +
	  ((self memoryAt: address + 2) <<  8) +
	  ((self memoryAt: address + 1) << 16) +
	  ((self memoryAt: address ) << 24).
		
	^ unsigned asSigned32
	


]

{ #category : #initialization }
Forth >> stackAsArray [

	^ memory stackAsArray
]

{ #category : #initialization }
Forth >> stackSize [

	^ memory stackSize
]

{ #category : #initialization }
Forth >> step [ 

	"('pc: ', pc asString, ' ', (self labels associations select: [ :e | e value = pc ]) asString)  traceCr."
	(self memoryAt: pc) execute.
	state = #running ifTrue: [ 
		pc := pc + 1. ]
]

{ #category : #initialization }
Forth >> toggleFlag: flag of: wordAddress [
	
	| flagAddress |
	flagAddress := wordAddress + self wordSize.
	self writeByte: ((self byteAt: flagAddress) bitXor: flag) at: flagAddress
]

{ #category : #accessing }
Forth >> unknownLabels [

	^ unknownLabels
]

{ #category : #accessing }
Forth >> uppercase [
	^ false
]

{ #category : #initialization }
Forth >> varHere [

	^ self addressForLabel: 'var_HERE'
]

{ #category : #initialization }
Forth >> varHereValue [

	^ self wordAt: self varHere
]

{ #category : #initialization }
Forth >> varHereValue: aValue [

	^ self writeWord: aValue at: self varHere
]

{ #category : #initialization }
Forth >> varLatest [

	^ self addressForLabel: 'var_LATEST'
]

{ #category : #initialization }
Forth >> varLatestValue [

	^ self wordAt: self varLatest
]

{ #category : #initialization }
Forth >> varLatestValue: aValue [

	^ self writeWord: aValue at: self varLatest
]

{ #category : #initialization }
Forth >> varState [

	^ self addressForLabel: 'var_STATE'
]

{ #category : #initialization }
Forth >> varStateValue [

	^ self signedWordAt: self varState
]

{ #category : #initialization }
Forth >> varStateValue: aValue [

	^ self writeWord: aValue at: self varState
]

{ #category : #initialization }
Forth >> wordAt: address [ 

	^ ((self memoryAt: address + 3)) +
	  ((self memoryAt: address + 2) <<  8) +
	  ((self memoryAt: address + 1) << 16) +
	  ((self memoryAt: address + 0) << 24) 
	


]

{ #category : #accessing }
Forth >> wordBufferAddress [

	^ self addressForLabel: 'word_buffer'
]

{ #category : #accessing }
Forth >> wordBufferSize [

	^ 32
]

{ #category : #initialization }
Forth >> wordSize [

	^ 4
]

{ #category : #initialization }
Forth >> writeByte: aByte at: address [ 

	self memoryAt: address put: aByte
	

]

{ #category : #initialization }
Forth >> writeCode: aCode at: address [ 

	self memoryAt: address put: aCode
	

]

{ #category : #initialization }
Forth >> writeWord: aWord at: address [ 

	| num bytes |
	
	num := aWord asUnsigned32.
	
	bytes := num asByteArrayOfSize: 4.

	self memoryAt: (address+0) put: (bytes at: 1).
	self memoryAt: (address+1) put: (bytes at: 2).
	self memoryAt: (address+2) put: (bytes at: 3).
	self memoryAt: (address+3) put: (bytes at: 4).
	


]
