Class {
	#name : #ForthTestExtendedWords,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #running }
ForthTestExtendedWords >> part1 [

	^ '\ -*- text -*-
\	A sometimes minimal FORTH compiler and tutorial for Linux / i386 systems. -*- asm -*-
\	By Richard W.M. Jones <rich@annexia.org> http://annexia.org/forth
\	This is PUBLIC DOMAIN (see public domain release statement below).
\	$Id: jonesforth.f,v 1.18 2009-09-11 08:32:33 rich Exp $
\
\	The first part of this tutorial is in jonesforth.S.  Get if from http://annexia.org/forth
\
\	PUBLIC DOMAIN ----------------------------------------------------------------------
\
\	I, the copyright holder of this work, hereby release it into the public domain. This applies worldwide.
\
\	In case this is not legally possible, I grant any entity the right to use this work for any purpose,
\	without any conditions, unless such conditions are required by law.
\
\	SETTING UP ----------------------------------------------------------------------
\
\	Let''s get a few housekeeping things out of the way.  Firstly because I need to draw lots of
\	ASCII-art diagrams to explain concepts, the best way to look at this is using a window which
\	uses a fixed width font and is at least this wide:
\
\<------------------------------------------------------------------------------------------------------------------------>
\
\	Secondly make sure TABS are set to 8 characters.  The following should be a vertical
\	line.  If not, sort out your tabs.
\
\		|
\	        |
\	    	|
\
\	Thirdly I assume that your screen is at least 50 characters high.
\
\	START OF FORTH CODE ----------------------------------------------------------------------
\
\	We''ve now reached the stage where the FORTH system is running and self-hosting.  All further
\	words can be written as FORTH itself, including words like IF, THEN, .", etc which in most
\	languages would be considered rather fundamental.
\
\	Some notes about the code:
\
\	I use indenting to show structure.  The amount of whitespace has no meaning to FORTH however
\	except that you must use at least one whitespace character between words, and words themselves
\	cannot contain whitespace.
\
\	FORTH is case-sensitive.  Use capslock!

\ The primitive word /MOD (DIVMOD) leaves both the quotient and the remainder on the stack.  (On
\ i386, the idivl instruction gives both anyway).  Now we can define the / and MOD in terms of /MOD
\ and a few other primitives.
: / /MOD SWAP DROP ;
: MOD /MOD DROP ;

\ Define some character constants
: ''\n'' 10 ;
: BL   32 ; \ BL (BLank) is a standard FORTH word for space.

\ CR prints a carriage return
: CR ''\n'' EMIT ;

\ SPACE prints a space
: SPACE BL EMIT ;

\ NEGATE leaves the negative of a number on the stack.
: NEGATE 0 SWAP - ;

\ Standard words for booleans.
: TRUE  1 ;
: FALSE 0 ;
: NOT   0= ;

\ LITERAL takes whatever is on the stack and compiles LIT <foo>
: LITERAL IMMEDIATE
	'' LIT ,		\ compile LIT
	,		\ compile the literal itself (from the stack)
	;

\ Now we can use [ and ] to insert literals which are calculated at compile time.  (Recall that
\ [ and ] are the FORTH words which switch into and out of immediate mode.)
\ Within definitions, use [ ... ] LITERAL anywhere that ''...'' is a constant expression which you
\ would rather only compute once (at compile time, rather than calculating it each time your word runs).
: '':''
	[		\ go into immediate mode (temporarily)
	CHAR :		\ push the number 58 (ASCII code of colon) on the parameter stack
	]		\ go back to compile mode
	LITERAL		\ compile LIT 58 as the definition of '':'' word
;

\ A few more character constants defined the same way as above.
: '';'' [ CHAR ; ] LITERAL ;
: ''('' [ CHAR ( ] LITERAL ;
: '')'' [ CHAR ) ] LITERAL ;
: ''"'' [ CHAR " ] LITERAL ;
: ''A'' [ CHAR A ] LITERAL ;
: ''0'' [ CHAR 0 ] LITERAL ;
: ''-'' [ CHAR - ] LITERAL ;
: ''.'' [ CHAR . ] LITERAL ;

\ While compiling, ''[COMPILE] word'' compiles ''word'' if it would otherwise be IMMEDIATE.
: [COMPILE] IMMEDIATE
	WORD		\ get the next word
	FIND		\ find it in the dictionary
	>CFA		\ get its codeword
	,		\ and compile that
;

\ RECURSE makes a recursive call to the current word that is being compiled.
\
\ Normally while a word is being compiled, it is marked HIDDEN so that references to the
\ same word within are calls to the previous definition of the word.  However we still have
\ access to the word which we are currently compiling through the LATEST pointer so we
\ can use that to compile a recursive call.
: RECURSE IMMEDIATE
	LATEST @	\ LATEST points to the word being compiled at the moment
	>CFA		\ get the codeword
	4-
	,		\ compile it
;

\	CONTROL STRUCTURES ----------------------------------------------------------------------
\
\ So far we have defined only very simple definitions.  Before we can go further, we really need to
\ make some control structures, like IF ... THEN and loops.  Luckily we can define arbitrary control
\ structures directly in FORTH.
\
\ Please note that the control structures as I have defined them here will only work inside compiled
\ words.  If you try to type in expressions using IF, etc. in immediate mode, then they won''t work.
\ Making these work in immediate mode is left as an exercise for the reader.

\ condition IF true-part THEN rest
\	-- compiles to: --> condition 0BRANCH OFFSET true-part rest
\	where OFFSET is the offset of ''rest''
\ condition IF true-part ELSE false-part THEN
\ 	-- compiles to: --> condition 0BRANCH OFFSET true-part BRANCH OFFSET2 false-part rest
\	where OFFSET if the offset of false-part and OFFSET2 is the offset of rest

\ IF is an IMMEDIATE word which compiles 0BRANCH followed by a dummy offset, and places
\ the address of the 0BRANCH on the stack.  Later when we see THEN, we pop that address
\ off the stack, calculate the offset, and back-fill the offset.
: IF IMMEDIATE
	'' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP -	\ calculate the offset from the address saved on the stack
	SWAP !		\ store the offset in the back-filled location
;

: ELSE IMMEDIATE
	'' BRANCH ,	\ definite branch to just over the false-part
	HERE @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
	SWAP		\ now back-fill the original (IF) offset
	DUP		\ same as for THEN word above
	HERE @ SWAP -
	SWAP !
;

\ BEGIN loop-part condition UNTIL
\	-- compiles to: --> loop-part condition 0BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ This is like do { loop-part } while (condition) in the C language
: BEGIN IMMEDIATE
	HERE @		\ save location on the stack
;

: UNTIL IMMEDIATE
	'' 0BRANCH ,	\ compile 0BRANCH
	HERE @ -	\ calculate the offset from the address saved on the stack
	,		\ compile the offset here
;

\ BEGIN loop-part AGAIN
\	-- compiles to: --> loop-part BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ In other words, an infinite loop which can only be returned from with EXIT
: AGAIN IMMEDIATE
	'' BRANCH ,	\ compile BRANCH
	HERE @ -	\ calculate the offset back
	,		\ compile the offset here
;

\ BEGIN condition WHILE loop-part REPEAT
\	-- compiles to: --> condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\	where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\ So this is like a while (condition) { loop-part } loop in the C language
: WHILE IMMEDIATE
	'' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset2 on the stack
	0 ,		\ compile a dummy offset2
;

: REPEAT IMMEDIATE
	'' BRANCH ,	\ compile BRANCH
	SWAP		\ get the original offset (from BEGIN)
	HERE @ - ,	\ and compile it after BRANCH
	DUP
	HERE @ SWAP -	\ calculate the offset2
	SWAP !		\ and back-fill it in the original location
;

\ UNLESS is the same as IF but the test is reversed.
\
\ Note the use of [COMPILE]: Since IF is IMMEDIATE we don''t want it to be executed while UNLESS
\ is compiling, but while UNLESS is running (which happens to be when whatever word using UNLESS is
\ being compiled -- whew!).  So we use [COMPILE] to reverse the effect of marking IF as immediate.
\ This trick is generally used when we want to write our own control words without having to
\ implement them all in terms of the primitives 0BRANCH and BRANCH, but instead reusing simpler
\ control words like (in this instance) IF.
: UNLESS IMMEDIATE
	'' NOT ,		\ compile NOT (to reverse the test)
	[COMPILE] IF	\ continue by calling the normal IF
;

\	COMMENTS ----------------------------------------------------------------------
\
\ FORTH allows ( ... ) as comments within function definitions.  This works by having an IMMEDIATE
\ word called ( which just drops input characters until it hits the corresponding ).
: ( IMMEDIATE
	1		\ allowed nested parens by keeping track of depth
	BEGIN
		KEY		\ read next character
		DUP ''('' = IF	\ open paren?
			DROP		\ drop the open paren
			1+		\ depth increases
		ELSE
			'')'' = IF	\ close paren?
				1-		\ depth decreases
			THEN
		THEN
	DUP 0= UNTIL		\ continue until we reach matching close paren, depth 0
	DROP		\ drop the depth counter
;

(
	From now on we can use ( ... ) for comments.

	STACK NOTATION ----------------------------------------------------------------------

	In FORTH style we can also use ( ... -- ... ) to show the effects that a word has on the
	parameter stack.  For example:

	( n -- )	means that the word consumes an integer (n) from the parameter stack.
	( b a -- c )	means that the word uses two integers (a and b, where a is at the top of stack)
				and returns a single integer (c).
	( -- )		means the word has no effect on the stack
)'
]

{ #category : #running }
ForthTestExtendedWords >> setUp [

	forth := Forth new init.
]

{ #category : #running }
ForthTestExtendedWords >> testBeginAgain [

	forth << '
	: if immediate '' 0branch , here @ 0 , ;
	: then immediate dup here @ swap - swap ! ;
	: begin immediate here @ ;
	: again immediate '' branch , here @ - , ;
	: test 10 begin dup 1- dup 0= if exit then again ; 
	test
	'.
	
	forth run.
	self assertStack: #(10 9 8 7 6 5 4 3 2 1 0).

]

{ #category : #running }
ForthTestExtendedWords >> testBeginUntil [

	forth << ': begin immediate here @ ;
	: until immediate '' 0branch , here @ - , ;
	: test 10 begin dup 1- dup 0= until ; 
	test
	'.
	
	forth run.
	self assertStack: #(10 9 8 7 6 5 4 3 2 1 0).

]

{ #category : #running }
ForthTestExtendedWords >> testBeginWhileRepeat [

	forth << '
	: begin immediate here @ ;
	: while immediate '' 0branch , here @ 0 , ;
	: repeat immediate '' branch , swap here @ - , dup here @ swap - swap ! ;
	: test 10 begin dup 0<> while dup 1- repeat ; 
	test
	'.
	
	forth run.
	self assertStack: #(10 9 8 7 6 5 4 3 2 1 0).

]

{ #category : #running }
ForthTestExtendedWords >> testBeginWhileRepeatUppercase [

	forth := ForthUppercase new init.

	forth << '
\ BEGIN loop-part condition UNTIL
\	-- compiles to: --> loop-part condition 0BRANCH OFFSET
\	where OFFSET points back to the loop-part
\ This is like do { loop-part } while (condition) in the C language
: BEGIN IMMEDIATE
	HERE @		\ save location on the stack
;


\ BEGIN condition WHILE loop-part REPEAT
\	-- compiles to: --> condition 0BRANCH OFFSET2 loop-part BRANCH OFFSET
\	where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\ So this is like a while (condition) { loop-part } loop in the C language
: WHILE IMMEDIATE
	'' 0BRANCH ,	\ compile 0BRANCH
	HERE @		\ save location of the offset2 on the stack
	0 ,		\ compile a dummy offset2
;	
	
: REPEAT IMMEDIATE
	'' BRANCH ,	\ compile BRANCH
	SWAP		\ get the original offset (from BEGIN)
	HERE @ - ,	\ and compile it after BRANCH
	DUP
	HERE @ SWAP -	\ calculate the offset2
	SWAP !		\ and back-fill it in the original location
;
	: TEST 10 BEGIN DUP 0<> WHILE DUP 1- REPEAT ; 
	TEST
	'.
	
	forth run.
	self assertStack: #(10 9 8 7 6 5 4 3 2 1 0).

]

{ #category : #running }
ForthTestExtendedWords >> testBooleans [

	forth << ': true  1 ;
: false 0 ;
: not   0= ;
true false true not false not'.
	
	forth run.
	self assertStack: #(1 0 0 1).

]

{ #category : #running }
ForthTestExtendedWords >> testChar [

	forth << 'char A'.
	
	forth run.
	self assertStack: #(65).

]

{ #category : #running }
ForthTestExtendedWords >> testComments [

	forth << '
	: literal immediate '' lit , 	, ; 
	: ''('' [ char ( ] literal ;
	: '')'' [ char ) ] literal ;
	: begin immediate here @ ;
	: until immediate '' 0branch , here @ - , ;
	: if immediate '' 0branch , here @ 0 , ;
	: then immediate dup here @ swap - swap ! ;
	: else immediate '' branch , here @ 0 , swap dup here @ swap - swap ! ;
	: ( immediate 1 begin key dup ''('' = if drop 1+ else '')'' = if 1- then then dup 0= until drop ;
	( this is a comment )
	1 2 3
	'.
	
	forth run.
	self assertStack: #(1 2 3).

]

{ #category : #running }
ForthTestExtendedWords >> testDSPStore [

	forth := ForthUppercase new init.
	forth << '1 4 DSP!'.
	forth run.
	self assertStack: #(1 0 0 0).
	
	forth := ForthUppercase new init.
	forth << '1 2 3 4 5 2 DSP!'.
	forth run.
	self assertStack: #(1 2)


]

{ #category : #running }
ForthTestExtendedWords >> testDepth [

	forth := ForthUppercase new init.

	forth << ';
: DEPTH
	DSP@ S0 @ -
;

	
1 2 3 DEPTH
	'.
	
	forth run.
	self assertStack: #(1 2 3 3)

]

{ #category : #running }
ForthTestExtendedWords >> testDiv [

	forth << ': / /mod swap drop ;
7  3 /'.
	
	forth run.
	self assertStack: #(2)
]

{ #category : #running }
ForthTestExtendedWords >> testDotS [

	forth := ForthUppercase new init.
	forth << self part1.

	forth << '
	
: HEX ( -- ) 16 BASE ! ;
	
( This is the underlying recursive definition of U. )
: U.		( u -- )
	BASE @ /MOD	( width rem quot ) 
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN

	( print the remainder )
	DUP 10 < IF
		''0''		( decimal digits 0..9 )
	ELSE
		10 -		( hex and beyond digits A..Z )
		''A''
	THEN
	+
	EMIT
;

: .S		( -- )
	S0 @ 		( get current stack pointer )
	DSP@ 
	2 -
	BEGIN
		2DUP <=
	WHILE
		SWAP DUP @DSP U.	( print the stack element )
		SPACE
		1+		( move up )
		SWAP
	REPEAT
	DROP
	DROP
;

1 2 3 4 .S
	'.
	
	forth run.
	self assertStack: #(1 2 3 4).
	self assertCollection: (String withAll: forth outputBuffer asByteArray) equals: '1 2 3 4 '

]

{ #category : #running }
ForthTestExtendedWords >> testEmitSpace [

	forth << ': ''\n'' 10 ;
: bl   32 ;
: cr ''\n'' emit ;
: space bl emit ;
cr space'.
	
	forth run.
	self assertStack: #().
	self assertCollection: forth outputBuffer hasSameElements: #(10 32).
]

{ #category : #running }
ForthTestExtendedWords >> testIfThenElse [

	forth << ': if immediate '' 0branch , here @ 0 , ;
	: then immediate dup here @ swap - swap ! ;
	: else immediate '' branch , here @ 0 , swap dup here @ swap - swap ! ;
	: test1 1 if 42 then ;
	: test2 0 if 42 then ;
	: test3 1 if 42 else 32 then ;
	: test4 0 if 42 else 32 then ;
	test1 -2 test2 -2 test3 -2 test4 -2
	'.
	
	forth run.
	self assertStack: #(42 -2 -2 42 -2 32 -2).

]

{ #category : #running }
ForthTestExtendedWords >> testItVariables [

	forth << '1 1 3 !'.
	
	forth run.
	self assertStack: #(1).

]

{ #category : #running }
ForthTestExtendedWords >> testJonesForth [ 

	forth := ForthUppercase new init.

	forth << ';
: DEPTH
	DSP@ S0 @ -
;

	
1 2 3 DEPTH
	'.
	
	forth run.
	self assertStack: #(1 2 3 3)

]

{ #category : #running }
ForthTestExtendedWords >> testLineComments [

	forth << '
	1 \ comment 2
	3
\ comment
4
	'.
	
	forth run.
	self assertStack: #(1 3 4).

]

{ #category : #running }
ForthTestExtendedWords >> testLiteral [

	forth << ': literal immediate '' lit , 	, ; 
: '':'' [	 char : ]	 literal ;
'':'' '.

	forth run.
	self assertStack: #(58).
	

]

{ #category : #running }
ForthTestExtendedWords >> testMod [

	forth << ': mod /mod drop ;
7  3 mod'.
	
	forth run.
	self assertStack: #(1)
]

{ #category : #running }
ForthTestExtendedWords >> testNegate [

	forth << ': negate 0 swap - ;
42 negate -42 negate'.
	
	forth run.
	self assertStack: #(-42 42).

]

{ #category : #running }
ForthTestExtendedWords >> testNip [

	forth := ForthUppercase new init.
	forth << self part1.

	forth << '
: NIP ( x y -- y ) SWAP DROP ;
	
1 2 3 NIP
	'.
	
	forth run.
	self assertStack: #(1 3)

]

{ #category : #running }
ForthTestExtendedWords >> testPick [

	forth := ForthUppercase new init.
	forth << self part1.

	forth << '
: PICK ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
   DSP@ SWAP -	( add to the stack pointer )
   2 -
	@DSP	( and fetch )
;	
1 2 6 3 4 5 3 PICK
	'.
	
	forth run.
	self assertStack: #(1 2 6 3 4 5 6)

]

{ #category : #running }
ForthTestExtendedWords >> testTick [

	forth << ': test '' drop ;
	test'.
	
	forth run.
	self assertStack: #(12).

]

{ #category : #running }
ForthTestExtendedWords >> testTuck [

	forth := ForthUppercase new init.
	forth << self part1.

	forth << '
: TUCK ( x y -- y x y ) SWAP OVER ;
	
1 2 3 TUCK
	'.
	
	forth run.
	self assertStack: #(1 3 2 3)

]

{ #category : #running }
ForthTestExtendedWords >> testUDot [

	forth := ForthUppercase new init.
	forth << self part1.

	forth << '
	
	: HEX ( -- ) 16 BASE ! ;
	
( This is the underlying recursive definition of U. )
: U.		( u -- )
	BASE @ /MOD	( width rem quot ) 
	?DUP IF			( if quotient <> 0 then )
		RECURSE		( print the quotient )
	THEN

	( print the remainder )
	DUP 10 < IF
		''0''		( decimal digits 0..9 )
	ELSE
		10 -		( hex and beyond digits A..Z )
		''A''
	THEN
	+
	EMIT
;
1 U. SPACE 9 U. SPACE 10 U. SPACE 10 HEX U. SPACE
	'.
	
	forth run.
	self assertStack: #().
	self assertCollection: (String withAll: forth outputBuffer asByteArray) equals: '1 9 10 A '

]
