
\  interpret quit halt execute penReset penUp penStep penSouth penRight penNorth penLeft penHome penDown penBusy char tell litstring 0branch branch ' hide hidden immediate ; : ] [ , create >dfa >cfa find number word emit key dsp! dsp@ rdrop rsp! rsp@ r> >r r0 f_immed f_hidden f_lenmask docol version s0 base latest here state exit cmove c@c! c@ c! -! +! @ ! lit invert xor or and 0>= 0<= 0> 0< 0<> 0= >= <= > < <> = /mod * - + 4- 4+ 1- 1+ ?dup 2swap 2dup 2drop -rot rot over dup swap drop 

: / /mod swap drop ;
: mod /mod drop ;

\ Define some character constants
: '\n' 10 ;
: bl   32 ; \ bl (BLank) is a standard FORTH word for space.

\ negate leaves the negative of a number on the stack.
: negate 0 swap - ;

\ Standard words for booleans.
: true  1 ;
: false 0 ;
: not   0= ;

\ literal takes whatever is on the stack and compiles lit <foo>
: literal immediate
	' lit ,		\ compile lit
	,		\ compile the literal itself (from the stack)
	;

\ Now we can use [ and ] to insert literals which are calculated at compile time.  (Recall that
\ [ and ] are the FORTH words which switch into and out of immediate mode.)
\ Within definitions, use [ ... ] literal anywhere that '...' is a constant expression which you
\ would rather only compute once (at compile time, rather than calculating it each time your word runs).
: ':'
	[		\ go into immediate mode (temporarily)
	char :		\ push the number 58 (ASCII code of colon) on the parameter stack
	]		\ go back to compile mode
	literal		\ compile lit 58 as the definition of ':' word
;

\ A few more character constants defined the same way as above.
: ';' [ char ; ] literal ;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'A' [ char A ] literal ;
: '0' [ char 0 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;

\ While compiling, '[compile] word' compiles 'word' if it would otherwise be immediate.
: [compile] immediate
	word		\ get the next word
	find		\ find it in the dictionary
	>cfa		\ get its codeword
	,		\ and compile that
;

\ recurse makes a recursive call to the current word that is being compiled.
\
\ Normally while a word is being compiled, it is marked hidden so that references to the
\ same word within are calls to the previous definition of the word.  However we still have
\ access to the word which we are currently compiling through the latest pointer so we
\ can use that to compile a recursive call.
: recurse immediate
	latest @	\ latest points to the word being compiled at the moment
	>cfa		\ get the codeword
	,		\ compile it
;

\	CONTROL STRUCTURES ----------------------------------------------------------------------
\
\ So far we have defined only very simple definitions.  Before we can go further, we really need to
\ make some control structures, like if ... then and loops.  Luckily we can define arbitrary control
\ structures directly in FORTH.
\
\ Please note that the control structures as I have defined them here will only work inside compiled
\ words.  If you try to type in expressions using if, etc. in immediate mode, then they won't work.
\ Making these work in immediate mode is left as an exercise for the reader.

\ condition if true-part then rest
\	-- compiles to: --> condition 0branch OFFSET true-part rest
\	where OFFSET is the offset of 'rest'
\ condition if true-part else false-part then
\ 	-- compiles to: --> condition 0branch OFFSET true-part branch OFFSET2 false-part rest
\	where OFFSET if the offset of false-part and OFFSET2 is the offset of rest

\ if is an immediate word which compiles 0branch followed by a dummy offset, and places
\ the address of the 0branch on the stack.  Later when we see then, we pop that address
\ off the stack, calculate the offset, and back-fill the offset.
: if immediate
	' 0branch ,	\ compile 0branch
	here @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
;

: then immediate
	dup
	here @ swap -	\ calculate the offset from the address saved on the stack
	swap !		\ store the offset in the back-filled location
;

: else immediate
	' branch ,	\ definite branch to just over the false-part
	here @		\ save location of the offset on the stack
	0 ,		\ compile a dummy offset
	swap		\ now back-fill the original (if) offset
	dup		\ same as for then word above
	here @ swap -
	swap !
;

\ begin loop-part condition until
\	-- compiles to: --> loop-part condition 0branch OFFSET
\	where OFFSET points back to the loop-part
\ This is like do { loop-part } while (condition) in the C language
: begin immediate
	here @		\ save location on the stack
;

: until immediate
	' 0branch ,	\ compile 0branch
	here @ -	\ calculate the offset from the address saved on the stack
	,		\ compile the offset here
;

\ begin loop-part again
\	-- compiles to: --> loop-part branch OFFSET
\	where OFFSET points back to the loop-part
\ In other words, an infinite loop which can only be returned from with exit
: again immediate
	' branch ,	\ compile branch
	here @ -	\ calculate the offset back
	,		\ compile the offset here
;

\ begin condition while loop-part repeat
\	-- compiles to: --> condition 0branch OFFSET2 loop-part branch OFFSET
\	where OFFSET points back to condition (the beginning) and OFFSET2 points to after the whole piece of code
\ So this is like a while (condition) { loop-part } loop in the C language
: while immediate
	' 0branch ,	\ compile 0branch
	here @		\ save location of the offset2 on the stack
	0 ,		\ compile a dummy offset2
;

: repeat immediate
	' branch ,	\ compile branch
	swap		\ get the original offset (from begin)
	here @ - ,	\ and compile it after branch
	dup
	here @ swap -	\ calculate the offset2
	swap !		\ and back-fill it in the original location
;

\ unless is the same as if but the test is reversed.
\
\ Note the use of [compile]: Since if is immediate we don't want it to be executed while unless
\ is compiling, but while unless is running (which happens to be when whatever word using unless is
\ being compiled -- whew!).  So we use [compile] to reverse the effect of marking if as immediate.
\ This trick is generally used when we want to write our own control words without having to
\ implement them all in terms of the primitives 0branch and branch, but instead reusing simpler
\ control words like (in this instance) if.
: unless immediate
	' not ,		\ compile not (to reverse the test)
	[compile] if	\ continue by calling the normal if
;

\	COMMENTS ----------------------------------------------------------------------
\
\ FORTH allows ( ... ) as comments within function definitions.  This works by having an immediate
\ word called ( which just drops input characters until it hits the corresponding ).
: ( immediate
	1		\ allowed nested parens by keeping track of depth
	begin
		key		\ read next character
		dup '(' = if	\ open paren?
			drop		\ drop the open paren
			1+		\ depth increases
		else
			')' = if	\ close paren?
				1-		\ depth decreases
			then
		then
	dup 0= until		\ continue until we reach matching close paren, depth 0
	drop		\ drop the depth counter
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
)

( Some more complicated stack examples, showing the stack notation. )
: nil ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
	1+		( add one because of 'u' on the stack )
	4 *		( multiply by the word size )
	dsp@ +		( add to the stack pointer )
	@    		( and fetch )
;

( Standard words for manipulating base. )
: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;


(
	CONSTANTS and VARIABLES ----------------------------------------------------------------------

	In FORTH, global constants and variables are defined like this:

	10 constant TEN		when TEN is executed, it leaves the integer 10 on the stack
	variable VAR		when VAR is executed, it leaves the address of VAR on the stack

	Constants can be read but not written, eg:

	TEN . cr		prints 10

	You can read a variable (in this example called VAR) by doing:

	VAR @			leaves the value of VAR on the stack
	VAR @ . cr		prints the value of VAR
	VAR ? cr		same as above, since ? is the same as @ .

	and update the variable by doing:

	20 VAR !		sets VAR to 20

	Note that variables are uninitialised (but see value later on which provides initialised
	variables with a slightly simpler syntax).

	How can we define the words constant and variable?

	The trick is to define a new word for the variable itself (eg. if the variable was called
	'VAR' then we would define a new word called VAR).  This is easy to do because we exposed
	dictionary entry creation through the create word (part of the definition of : above).
	A call to word [TEN] create (where [TEN] means that "TEN" is the next word in the input)
	leaves the dictionary entry:

				   +--- here
				   |
				   V
	+---------+---+---+---+---+
	| LINK    | 3 | T | E | N |
	+---------+---+---+---+---+
                   len

	For constant we can continue by appending docol (the codeword), then lit followed by
	the constant itself and then exit, forming a little word definition that returns the
	constant:

	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | T | E | N | docol      | lit        | 10         | exit       |
	+---------+---+---+---+---+------------+------------+------------+------------+
                   len              codeword

	Notice that this word definition is exactly the same as you would have got if you had
	written : TEN 10 ;

	Note for people reading the code below: docol is a constant word which we defined in the
	assembler part which returns the value of the assembler symbol of the same name.
)
: constant
	word		( get the name (the name follows constant) )
	create		( make the dictionary entry )
	docol ,		( append docol (the codeword field of this word) )
	' lit ,		( append the codeword lit )
	,		( append the value on the top of the stack )
	' exit ,	( append the codeword exit )
;

(
	variable is a little bit harder because we need somewhere to put the variable.  There is
	nothing particularly special about the user memory (the area of memory pointed to by here
	where we have previously just stored new word definitions).  We can slice off bits of this
	memory area to store anything we want, so one possible definition of variable might create
	this:

	   +--------------------------------------------------------------+
	   |								  |
	   V								  |
	+---------+---------+---+---+---+---+------------+------------+---|--------+------------+
	| <var>   | LINK    | 3 | V | A | R | docol      | lit        | <addr var> | exit       |
	+---------+---------+---+---+---+---+------------+------------+------------+------------+
        		     len              codeword

	where <var> is the place to store the variable, and <addr var> points back to it.

	To make this more general let's define a couple of words which we can use to allocate
	arbitrary memory from the user memory.

	First allot, where n allot allocates n bytes of memory.  (Note when calling this that
	it's a very good idea to make sure that n is a multiple of 4, or at least that next time
	a word is compiled that here has been left as a multiple of 4).
)
: allot		( n -- addr )
	here @ swap	( here n )
	here +!		( adds n to here, after this the old value of here is still on the stack )
;

(
	Second, cells.  In FORTH the phrase 'n cells allot' means allocate n integers of whatever size
	is the natural size for integers on this machine architecture.  On this 32 bit machine therefore
	cells just multiplies the top of stack by 4.
)
: cells ( n -- n ) 4 * ;

(
	So now we can define variable easily in much the same way as constant above.  Refer to the
	diagram above to see what the word that this creates will look like.
)
: variable
	1 cells allot	( allocate 1 cell of memory, push the pointer to this memory )
	word create	( make the dictionary entry (the name follows variable) )
	docol ,		( append docol (the codeword field of this word) )
	' lit ,		( append the codeword lit )
	,		( append the pointer to the new memory )
	' exit ,	( append the codeword exit )
;

(
	VALUES ----------------------------------------------------------------------

	VALUEs are like VARIABLEs but with a simpler syntax.  You would generally use them when you
	want a variable which is read often, and written infrequently.

	20 value VAL 	creates VAL with initial value 20
	VAL		pushes the value (20) directly on the stack
	30 to VAL	updates VAL, setting it to 30
	VAL		pushes the value (30) directly on the stack

	Notice that 'VAL' on its own doesn't return the address of the value, but the value itself,
	making values simpler and more obvious to use than variables (no indirection through '@').
	The price is a more complicated implementation, although despite the complexity there is no
	performance penalty at runtime.

	A naive implementation of 'to' would be quite slow, involving a dictionary search each time.
	But because this is FORTH we have complete control of the compiler so we can compile to more
	efficiently, turning:
		to VAL
	into:
		lit <addr> !
	and calculating <addr> (the address of the value) at compile time.

	Now this is the clever bit.  We'll compile our value like this:

	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | V | A | L | docol      | lit        | <value>    | exit       |
	+---------+---+---+---+---+------------+------------+------------+------------+
                   len              codeword

	where <value> is the actual value itself.  Note that when VAL executes, it will push the
	value on the stack, which is what we want.

	But what will to use for the address <addr>?  Why of course a pointer to that <value>:

		code compiled	- - - - --+------------+------------+------------+-- - - - -
		by to VAL		  | lit        | <addr>     | !          |
				- - - - --+------------+-----|------+------------+-- - - - -
							     |
							     V
	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | V | A | L | docol      | lit        | <value>    | exit       |
	+---------+---+---+---+---+------------+------------+------------+------------+
                   len              codeword

	In other words, this is a kind of self-modifying code.

	(Note to the people who want to modify this FORTH to add inlining: values defined this
	way cannot be inlined).
)
: value		( n -- )
	word create	( make the dictionary entry (the name follows value) )
	docol ,		( append docol )
	' lit ,		( append the codeword lit )
	,		( append the initial value )
	' exit ,	( append the codeword exit )
;

: to immediate	( n -- )
	word		( get the name of the value )
	find		( look it up in the dictionary )
	>dfa		( get a pointer to the first data field (the 'lit') )
	4+		( increment to point at the value )
	state @ if	( compiling? )
		' lit ,		( compile lit )
		,		( compile the address of the value )
		' ! ,		( compile ! )
	else		( immediate mode )
		!		( update it straightaway )
	then
;

( x +to VAL adds x to VAL )
: +to immediate
	word		( get the name of the value )
	find		( look it up in the dictionary )
	>dfa		( get a pointer to the first data field (the 'lit') )
	4+		( increment to point at the value )
	state @ if	( compiling? )
		' lit ,		( compile lit )
		,		( compile the address of the value )
		' +! ,		( compile +! )
	else		( immediate mode )
		+!		( update it straightaway )
	then
;



: variable: variable latest @ >cfa execute ! ;
: ->cell 4 swap +! ;
: <-cell 4 swap -! ;

32 cells allot variable: loopSP
loopSP @ constant loopTop
: >loop loopSP @ ! loopSP ->cell ;
: loop> loopSP <-cell loopSP @ @ ; 
: do immediate ' >loop , ' >loop , [compile] begin ;
: loopCheck loop> loop> 1+  2dup =  -rot >loop >loop ;
: loopFinish loop> drop loop> drop ;
: loop immediate ' loopCheck , [compile] until ' loopFinish , ;

: mm 10 * ;
: cm 100 * ;
: dot ;
: dots ;
: pt 2 dots * ;

: PENWEST penLeft ; 
: PENEAST penRight ;

800 value maxPlotterX 
800 value maxPlotterY

variable plotterX
0 plotterX !
variable plotterY
0 plotterY !

: penTranslate begin dup 0> while penStep 1- repeat drop ;
: penMove penUp penTranslate ;
: penDraw penDown penTranslate ;

: abs dup 0< if dup dup + - then ;
 
: validPlotterX dup 0>= swap maxPlotterX <= and ;
: validPlotterY dup 0>= swap maxPlotterY <= and ;

: validatePlotterY plotterY @ swap pt + validPlotterY ;
: updatePlotterY pt plotterY +! ;
: validatePlotterX plotterX @ swap pt + validPlotterX ;
: updatePlotterX pt plotterX +! ;
: drawPt 1 pt penDraw ;
: movePt 1 pt penMove ;

: setDirectionNorth -1 validatePlotterY dup if penNorth -1 updatePlotterY then ;
: setDirectionSouth  1 validatePlotterY dup if penSouth  1 updatePlotterY then ;
: setDirectionEast   1 validatePlotterX dup if PENEAST   1 updatePlotterX then ;
: setDirectionWest  -1 validatePlotterX dup if PENWEST  -1 updatePlotterX then ;

: drawIfPossible if drawPt then ;
: moveIfPossible if movePt then ;
: drawIfBothPossible or if drawPt then ;
: moveIfBothPossible or if movePt then ;

: drN  penReset setDirectionNorth drawIfPossible ;
: drS  penReset setDirectionSouth drawIfPossible ;
: drW  penReset setDirectionWest  drawIfPossible ;
: drE  penReset setDirectionEast  drawIfPossible ;
: drNE penReset setDirectionNorth setDirectionEast drawIfBothPossible ;
: drNW penReset setDirectionNorth setDirectionWest drawIfBothPossible ;
: drSE penReset setDirectionSouth setDirectionEast drawIfBothPossible ;
: drSW penReset setDirectionSouth setDirectionWest drawIfBothPossible ;

: moveN  penReset setDirectionNorth moveIfPossible ;
: moveS  penReset setDirectionSouth moveIfPossible ;
: moveW  penReset setDirectionWest  moveIfPossible ;
: moveE  penReset setDirectionEast  moveIfPossible ;	
: moveNE penReset setDirectionNorth setDirectionEast moveIfBothPossible ;
: moveNW penReset setDirectionNorth setDirectionWest moveIfBothPossible ;
: moveSE penReset setDirectionSouth setDirectionEast moveIfBothPossible ;
: moveSW penReset setDirectionSouth setDirectionWest moveIfBothPossible ;

: penJump 
   dup 0 > if dup 0 do moveS loop then
   dup 0 < if dup abs 0 do moveN loop then 
   drop
   dup 0 > if dup 0 do moveE loop then
   dup 0 < if dup abs 0 do moveW loop then
 	drop
	;

: letterOffset moveE ;

: carriageReturn PENWEST plotterX @ penTranslate penReset 0 plotterX ! ;
: lineFeed 9 0 do moveS loop ;
: cr carriageReturn lineFeed ;


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
: letter'
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
	cr          (  10 )
	letterNone  (  11 )
	letterNone  (  12 )
	cr       (  13 )
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
	letter'    (  39 )
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

: printAscii  4 * ' characterTable 4+ + @ execute ;

: emit
    dup emit 
	plotterX @ 64 5 pt * > if '\n' printAscii then 
	dup 127 > if letterNone else printAscii then ;

\ cr prints a carriage return
: cr '\n' emit ;

\ space prints a space
: space bl emit ;

( With the looping constructs, we can now write spaces, which writes n spaces to stdout. )
: spaces	( n -- )
	begin
		dup 0>		( while n > 0 )
	while
		space		( print a space )
		1-		( until we count down to 0 )
	repeat
	drop
;

(
	aligned takes an address and rounds it up (aligns it) to the next 4 byte boundary.
)
: aligned	( addr -- addr )
	3 + 3 invert and	( (addr+3) & ~3 )
;

(
	align aligns the here pointer, so the next word appended will be aligned properly.
)
: align here @ aligned here ! ;

(
	STRINGS ----------------------------------------------------------------------

	s" string" is used in FORTH to define strings.  It leaves the address of the string and
	its length on the stack, (length at the top of stack).  The space following s" is the normal
	space between FORTH words and is not a part of the string.

	This is tricky to define because it has to do different things depending on whether
	we are compiling or in immediate mode.  (Thus the word is marked immediate so it can
	detect this and do different things).

	In compile mode we append
		litstring <string length> <string rounded up 4 bytes>
	to the current word.  The primitive litstring does the right thing when the current
	word is executed.

	In immediate mode there isn't a particularly good place to put the string, but in this
	case we put the string at here (but we _don't_ change here).  This is meant as a temporary
	location, likely to be overwritten soon after.
)
( c, appends a byte to the current compiled word. )
: c,
	here @ c!	( store the character in the compiled image )
	1 here +!	( increment here pointer by 1 byte )
;

: s" immediate		( -- addr len )
	state @ if	( compiling? )
		' litstring ,	( compile litstring )
		here @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		begin
			key 		( get next character of the string )
			dup '"' <>
		while
			c,		( copy character )
		repeat
		drop		( drop the double quote character at the end )
		dup		( get the saved address of the length word )
		here @ swap -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		swap !		( and back-fill the length location )
		align		( round up to next multiple of 4 bytes for the remaining code )
	else		( immediate mode )
		here @		( get the start address of the temporary space )
		begin
			key
			dup '"' <>
		while
			over c!		( save next character )
			1+		( increment address )
		repeat
		drop		( drop the final " character )
		here @ -	( calculate the length )
		here @		( push the start address )
		swap 		( addr len )
	then
;





(
	PRINTING NUMBERS ----------------------------------------------------------------------

	The standard FORTH word . (DOT) is very important.  It takes the number at the top
	of the stack and prints it out.  However first I'm going to implement some lower-level
	FORTH words:

	u.r	( u width -- )	which prints an unsigned number, padded to a certain width
	u.	( u -- )	which prints an unsigned number
	.r	( n width -- )	which prints a signed number, padded to a certain width.

	For example:
		-123 6 .r
	will print out these characters:
		<space> <space> - 1 2 3

	In other words, the number padded left to a certain number of characters.

	The full number is printed even if it is wider than width, and this is what allows us to
	define the ordinary functions u. and . (we just set width to zero knowing that the full
	number will be printed anyway).

	Another wrinkle of . and friends is that they obey the current base in the variable base.
	base can be anything in the range 2 to 36.

	While we're defining . &c we can also define .s which is a useful debugging tool.  This
	word prints the current stack (non-destructively) from top to bottom.
)

( This is the underlying recursive definition of u. )
: u.		( u -- )
	base @ /mod	( width rem quot )
	?dup if			( if quotient <> 0 then )
		recurse		( print the quotient )
	then

	( print the remainder )
	dup 10 < if
		'0'		( decimal digits 0..9 )
	else
		10 -		( hex and beyond digits A..Z )
		'A'
	then
	+
	emit
;

(
	FORTH word .s prints the contents of the stack.  It doesn't alter the stack.
	Very useful for debugging.
)
: .s		( -- )
	dsp@		( get current stack pointer )
	begin
		dup s0 @ <
	while
		dup @ u.	( print the stack element )
		space
		4+		( move up )
	repeat
	drop
;

( This word returns the width (in characters) of an unsigned number in the current base )
: uwidth	( u -- width )
	base @ /	( rem quot )
	?dup if		( if quotient <> 0 then )
		recurse 1+	( return 1+recursive call )
	else
		1		( return 1 )
	then
;

: u.r		( u width -- )
	swap		( width u )
	dup		( width u u )
	uwidth		( width u uwidth )
	rot		( u uwidth width )
	swap -		( u width-uwidth )
	( At this point if the requested width is narrower, we'll have a negative number on the stack.
	  Otherwise the number on the stack is the number of spaces to print.  But spaces won't print
	  a negative number of spaces anyway, so it's now safe to call spaces ... )
	spaces
	( ... and then call the underlying implementation of u. )
	u.
;

(
	.r prints a signed number, padded to a certain width.  We can't just print the sign
	and call u.r because we want the sign to be next to the number ('-123' instead of '-  123').
)
: .r		( n width -- )
	swap		( width n )
	dup 0< if
		negate		( width u )
		1		( save a flag to remember that it was negative | width n 1 )
		swap		( width 1 u )
		rot		( 1 u width )
		1-		( 1 u width-1 )
	else
		0		( width u 0 )
		swap		( width 0 u )
		rot		( 0 u width )
	then
	swap		( flag width u )
	dup		( flag width u u )
	uwidth		( flag width u uwidth )
	rot		( flag u uwidth width )
	swap -		( flag u width-uwidth )

	spaces		( flag u )
	swap		( u flag )

	if			( was it negative? print the - character )
		'-' emit
	then

	u.
;

( Finally we can define word . in terms of .r, with a trailing space. )
: . 0 .r space ;

( The real U., note the trailing space. )
: u. u. space ;

( ? fetches the integer at an address and prints it. )
: ? ( addr -- ) @ . ;

( c a b within returns true if a <= c and c < b )
(  or define without ifs: over - >r - r>  U<  )
: within
	-rot		( b c a )
	over		( b c a c )
	<= if
		> if		( b c -- )
			true
		else
			false
		then
	else
		2drop		( b c -- )
		false
	then
;

( depth returns the depth of the stack. )
: depth		( -- n )
	s0 @ dsp@ -
	4-			( adjust because s0 was on the stack when we pushed DSP )
;

(
	." is the print string operator in FORTH.  Example: ." Something to print"
	The space after the operator is the ordinary space required between words and is not
	a part of what is printed.

	In immediate mode we just keep reading characters and printing them until we get to
	the next double quote.

	In compile mode we use s" to store the string, then add tell afterwards:
		litstring <string length> <string rounded up to 4 bytes> tell

	It may be interesting to note the use of [compile] to turn the call to the immediate
	word s" into compilation of that word.  It compiles it into the definition of .",
	not into the definition of the word being compiled when this is running (complicated
	enough for you?)
)
: ." immediate		( -- )
	state @ if	( compiling? )
		[compile] s"	( read the string, and compile litstring, etc. )
		' tell ,	( compile the final tell )
	else
		( In immediate mode, just read characters and print them until we get
		  to the ending double quote. )
		begin
			key
			dup '"' = if
				drop	( drop the double quote character )
				exit	( return from this function )
			then
			emit
		again
	then
;



(
	PRINTING THE DICTIONARY ----------------------------------------------------------------------

	id. takes an address of a dictionary entry and prints the word's name.

	For example: latest @ id. would print the name of the last word that was defined.
)
: id.
	4+		( skip over the link pointer )
	dup c@		( get the flags/length byte )
	f_lenmask and	( mask out the flags - just want the length )

	begin
		dup 0>		( length > 0? )
	while
		swap 1+		( addr len -- len addr+1 )
		dup c@		( len addr -- len addr char | get the next character)
		emit		( len addr char -- len addr | and print it)
		swap 1-		( len addr -- addr len-1    | subtract one from length )
	repeat
	2drop		( len addr -- )
;

(
	'word word find ?hidden' returns true if 'word' is flagged as hidden.

	'word word find ?immediate' returns true if 'word' is flagged as immediate.
)
: ?hidden
	4+		( skip over the link pointer )
	c@		( get the flags/length byte )
	f_hidden and	( mask the f_hidden flag and return it (as a truth value) )
;
: ?immediate
	4+		( skip over the link pointer )
	c@		( get the flags/length byte )
	f_immed and	( mask the f_immed flag and return it (as a truth value) )
;

(
	words prints all the words defined in the dictionary, starting with the word defined most recently.
	However it doesn't print hidden words.

	The implementation simply iterates backwards from latest using the link pointers.
)
: words
	latest @	( start at latest dictionary entry )
	begin
		?dup		( while link pointer is not null )
	while
		dup ?hidden not if	( ignore hidden words )
			dup id.		( but if not hidden, print the word )
			space
		then
		@		( dereference the link pointer - go to previous word )
	repeat
	cr
;

(
	forget ----------------------------------------------------------------------

	So far we have only allocated words and memory.  FORTH provides a rather primitive method
	to deallocate.

	'forget word' deletes the definition of 'word' from the dictionary and everything defined
	after it, including any variables and other memory allocated after.

	The implementation is very simple - we look up the word (which returns the dictionary entry
	address).  Then we set here to point to that address, so in effect all future allocations
	and definitions will overwrite memory starting at the word.  We also need to set latest to
	point to the previous word.

	Note that you cannot forget built-in words (well, you can try but it will probably cause
	a segfault).

	XXX: Because we wrote variable to store the variable in memory allocated before the word,
	in the current implementation variable FOO forget FOO will leak 1 cell of memory.
)
: forget
	word find	( find the word, gets the dictionary entry address )
	dup @ latest !	( set latest to point to the previous word )
	here !		( and store here with the dictionary address )
;

(
	dump ----------------------------------------------------------------------

	dump is used to dump out the contents of memory, in the 'traditional' hexdump format.

	Notice that the parameters to dump (address, length) are compatible with string words
	such as word and S".

	You can dump out the raw code for the last word you defined by doing something like:

		latest @ 128 dump
)
: dump		( addr len -- )
	base @ -rot		( save the current base at the bottom of the stack )
	hex			( and switch to hexadecimal mode )

	begin
		?dup		( while len > 0 )
	while
		over 8 u.r	( print the address )
		space

		( print up to 16 words on this line )
		2dup		( addr len addr len )
		1- 15 and 1+	( addr len addr linelen )
		begin
			?dup		( while linelen > 0 )
		while
			swap		( addr len linelen addr )
			dup c@		( addr len linelen addr byte )
			2 .r space	( print the byte )
			1+ swap 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		repeat
		drop		( addr len )

		( print the ASCII equivalents )
		2dup 1- 15 and 1+ ( addr len addr linelen )
		begin
			?dup		( while linelen > 0)
		while
			swap		( addr len linelen addr )
			dup c@		( addr len linelen addr byte )
			dup 32 128 within if	( 32 <= c < 128? )
				emit
			else
				drop '.' emit
			then
			1+ swap 1-	( addr len linelen addr -- addr len addr+1 linelen-1 )
		repeat
		drop		( addr len )
		cr

		dup 1- 15 and 1+ ( addr len linelen )
		tuck		( addr linelen len linelen )
		-		( addr linelen len-linelen )
		>r + r>		( addr+linelen len-linelen )
	repeat

	drop			( restore stack )
	base !			( restore saved base )
;

(
	case ----------------------------------------------------------------------

	case...endCase is how we do switch statements in FORTH.  There is no generally
	agreed syntax for this, so I've gone for the syntax mandated by the ISO standard
	FORTH (ANS-FORTH).

		( some value on the stack )
		case
		test1 of ... endOf
		test2 of ... endOf
		testn of ... endOf
		... ( default case )
		endCase

	The case statement tests the value on the stack by comparing it for equality with
	test1, test2, ..., testn and executes the matching piece of code within of ... endOf.
	If none of the test values match then the default case is executed.  Inside the ... of
	the default case, the value is still at the top of stack (it is implicitly drop-ed
	by endCase).  When endOf is executed it jumps after endCase (ie. there is no "fall-through"
	and no need for a break statement like in C).

	The default case may be omitted.  In fact the tests may also be omitted so that you
	just have a default case, although this is probably not very useful.

	An example (assuming that 'q', etc. are words which push the ASCII value of the letter
	on the stack):

		0 value quit
		0 value SLEEP
		key case
			'q' of 1 to quit endOf
			's' of 1 to SLEEP endOf
			( default case: )
			." Sorry, I didn't understand key <" dup emit ." >, try again." cr
		endCase

	(In some versions of FORTH, more advanced tests are supported, such as ranges, etc.
	Other versions of FORTH need you to write OTHERWISE to indicate the default case.
	As I said above, this FORTH tries to follow the ANS FORTH standard).

	The implementation of case...endCase is somewhat non-trivial.  I'm following the
	implementations from here:
	http://www.uni-giessen.de/faq/archiv/forthfaq.case_endcase/msg00000.html

	The general plan is to compile the code as a series of if statements:

	case				(push 0 on the immediate-mode parameter stack)
	test1 of ... endOf		test1 over = if drop ... else
	test2 of ... endOf		test2 over = if drop ... else
	testn of ... endOf		testn over = if drop ... else
	... ( default case )		...
	endCase				drop then [then [then ...]]

	The case statement pushes 0 on the immediate-mode parameter stack, and that number
	is used to count how many then statements we need when we get to endCase so that each
	if has a matching then.  The counting is done implicitly.  If you recall from the
	implementation above of if, each if pushes a code address on the immediate-mode stack,
	and these addresses are non-zero, so by the time we get to endCase the stack contains
	some number of non-zeroes, followed by a zero.  The number of non-zeroes is how many
	times if has been called, so how many times we need to match it with then.

	This code uses [compile] so that we compile calls to if, else, then instead of
	actually calling them while we're compiling the words below.

	As is the case with all of our control structures, they only work within word
	definitions, not in immediate mode.
)
: case immediate
	0		( push 0 to mark the bottom of the stack )
;

: of immediate
	' over ,	( compile over )
	' = ,		( compile = )
	[compile] if	( compile if )
	' drop ,  	( compile drop )
;

: endOf immediate
	[compile] else	( endOf is the same as else )
;

: endCase immediate
	' drop ,	( compile drop )

	( keep compiling then until we get to our zero marker )
	begin
		?dup
	while
		[compile] then
	repeat
;

(
	DECOMPILER ----------------------------------------------------------------------

	cfa> is the opposite of >cfa.  It takes a codeword and tries to find the matching
	dictionary definition.  (In truth, it works with any pointer into a word, not just
	the codeword pointer, and this is needed to do stack traces).

	In this FORTH this is not so easy.  In fact we have to search through the dictionary
	because we don't have a convenient back-pointer (as is often the case in other versions
	of FORTH).  Because of this search, cfa> should not be used when performance is critical,
	so it is only used for debugging tools such as the decompiler and printing stack
	traces.

	This word returns 0 if it doesn't find a match.
)
: cfa>
	latest @	( start at latest dictionary entry )
	begin
		?dup		( while link pointer is not null )
	while
		2dup swap	( cfa curr curr cfa )
		< if		( current dictionary entry < cfa? )
			nil		( leave curr dictionary entry on the stack )
			exit
		then
		@		( follow link pointer back )
	repeat
	drop		( restore stack )
	0		( sorry, nothing found )
;

(
	see decompiles a FORTH word.

	We search for the dictionary entry of the word, then search again for the next
	word (effectively, the end of the compiled word).  This results in two pointers:

	+---------+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 3 | T | E | N | docol      | lit        | 10         | exit       |
	+---------+---+---+---+---+------------+------------+------------+------------+
	 ^									       ^
	 |									       |
	Start of word							      End of word

	With this information we can have a go at decompiling the word.  We need to
	recognise "meta-words" like lit, litstring, branch, etc. and treat those separately.
)
: see
	word find	( find the dictionary entry to decompile )

	( Now we search again, looking for the next word in the dictionary.  This gives us
	  the length of the word that we will be decompiling.  (Well, mostly it does). )
	here @		( address of the end of the last compiled word )
	latest @	( word last curr )
	begin
		2 pick		( word last curr word )
		over		( word last curr word curr )
		<>		( word last curr word<>curr? )
	while			( word last curr )
		nil		( word curr )
		dup @		( word curr prev (which becomes: word last curr) )
	repeat

	drop		( at this point, the stack is: start-of-word end-of-word )
	swap		( end-of-word start-of-word )

	( begin the definition with : NAME [immediate] )
	':' emit space dup id. space
	dup ?immediate if ." immediate " then

	>dfa		( get the data address, ie. points after docol | end-of-word start-of-data )

	( now we start decompiling until we hit the end of the word )
	begin		( end start )
		2dup >
	while
		dup @		( end start codeword )

		case
		' lit of		( is it lit ? )
			4 + dup @		( get next word which is the integer constant )
			.			( and print it )
		endOf
		' litstring of		( is it litstring ? )
			[ char S ] literal emit '"' emit space ( print S"<space> )
			4 + dup @		( get the length word )
			swap 4 + swap		( end start+4 length )
			2dup tell		( print the string )
			'"' emit space		( finish the string with a final quote )
			+ aligned		( end start+4+len, aligned )
			4 -			( because we're about to add 4 below )
		endOf
		' 0branch of		( is it 0branch ? )
			." 0branch ( "
			4 + dup @		( print the offset )
			.
			." ) "
		endOf
		' branch of		( is it branch ? )
			." branch ( "
			4 + dup @		( print the offset )
			.
			." ) "
		endOf
		' ' of			( is it ' (TICK) ? )
			[ char ' ] literal emit space
			4 + dup @		( get the next codeword )
			cfa>			( and force it to be printed as a dictionary entry )
			id. space
		endOf
		' exit of		( is it exit? )
			( We expect the last word to be exit, and if it is then we don't print it
			  because exit is normally implied by ;.  exit can also appear in the middle
			  of words, and then it needs to be printed. )
			2dup			( end start end start )
			4 +			( end start end start+4 )
			<> if			( end start | we're not at the end )
				." exit "
			then
		endOf
					( default case: )
			dup			( in the default case we always need to dup before using )
			cfa>			( look up the codeword to get the dictionary entry )
			id. space		( and print it )
		endCase

		4 +		( end start+4 )
	repeat

	';' emit cr

	2drop		( restore stack )
;

(
	EXECUTION TOKENS ----------------------------------------------------------------------

	Standard FORTH defines a concept called an 'execution token' (or 'xt') which is very
	similar to a function pointer in C.  We map the execution token to a codeword address.

			execution token of DOUBLE is the address of this codeword
						    |
						    V
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
	| LINK    | 6 | D | O | U | B | L | E | 0 | docol      | dup        | +          | exit       |
	+---------+---+---+---+---+---+---+---+---+------------+------------+------------+------------+
                   len                         pad  codeword					       ^

	There is one assembler primitive for execution tokens, execute ( xt -- ), which runs them.

	You can make an execution token for an existing word the long way using >cfa,
	ie: word [foo] find >cfa will push the xt for foo onto the stack where foo is the
	next word in input.  So a very slow way to run DOUBLE might be:

		: DOUBLE dup + ;
		: SLOW word find >cfa execute ;
		5 SLOW DOUBLE . cr	\ prints 10

	We also offer a simpler and faster way to get the execution token of any word FOO:

		['] FOO

	(Exercises for readers: (1) What is the difference between ['] FOO and ' FOO?
	(2) What is the relationship between ', ['] and lit?)

	More useful is to define anonymous words and/or to assign xt's to variables.

	To define an anonymous word (and push its xt on the stack) use :noname ... ; as in this
	example:

		:noname ." anon word was called" cr ;	\ pushes xt on the stack
		dup execute execute			\ executes the anon word twice

	Stack parameters work as expected:

		:noname ." called with parameter " . cr ;
		dup
		10 swap execute		\ prints 'called with parameter 10'
		20 swap execute		\ prints 'called with parameter 20'

	Notice that the above code has a memory leak: the anonymous word is still compiled
	into the data segment, so even if you lose track of the xt, the word continues to
	occupy memory.  A good way to keep track of the xt and thus avoid the memory leak is
	to assign it to a constant, variable or value:

		0 value ANON
		:noname ." anon word was called" cr ; to ANON
		ANON execute
		ANON execute

	Another use of :noname is to create an array of functions which can be called quickly
	(think: fast switch statement).  This example is adapted from the ANS FORTH standard:

		10 cells allot constant CMD-TABLE
		: SET-CMD cells CMD-TABLE + ! ;
		: CALL-CMD cells CMD-TABLE + @ execute ;

		:noname ." alternate 0 was called" cr ;	 0 SET-CMD
		:noname ." alternate 1 was called" cr ;	 1 SET-CMD
			\ etc...
		:noname ." alternate 9 was called" cr ;	 9 SET-CMD

		0 CALL-CMD
		1 CALL-CMD
)

: :noname
	0 0 create	( create a word with no name - we need a dictionary header because ; expects it )
	here @		( current here value is the address of the codeword, ie. the xt )
	docol ,		( compile docol (the codeword) )
	]		( go into compile mode )
;

: ['] immediate
	' lit ,		( compile lit )
;

(
	EXCEPTIONS ----------------------------------------------------------------------

	Amazingly enough, exceptions can be implemented directly in FORTH, in fact rather easily.

	The general usage is as follows:

		: FOO ( n -- ) throw ;

		: TEST-EXCEPTIONS
			25 ['] FOO catch	\ execute 25 FOO, catching any exception
			?dup if
				." called FOO and it threw exception number: "
				. cr
				drop		\ we have to drop the argument of FOO (25)
			then
		;
		\ prints: called FOO and it threw exception number: 25

	catch runs an execution token and detects whether it throws any exception or not.  The
	stack signature of catch is rather complicated:

		( a_n-1 ... a_1 a_0 xt -- r_m-1 ... r_1 r_0 0 )		if xt did not throw an exception
		( a_n-1 ... a_1 a_0 xt -- ?_n-1 ... ?_1 ?_0 e )		if xt DID throw exception 'e'

	where a_i and r_i are the (arbitrary number of) argument and return stack contents
	before and after xt is EXECUTEd.  Notice in particular the case where an exception
	is thrown, the stack pointer is restored so that there are n of _something_ on the
	stack in the positions where the arguments a_i used to be.  We don't really guarantee
	what is on the stack -- perhaps the original arguments, and perhaps other nonsense --
	it largely depends on the implementation of the word that was executed.

	throw, abort and a few others throw exceptions.

	Exception numbers are non-zero integers.  By convention the positive numbers can be used
	for app-specific exceptions and the negative numbers have certain meanings defined in
	the ANS FORTH standard.  (For example, -1 is the exception thrown by abort).

	0 throw does nothing.  This is the stack signature of throw:

		( 0 -- )
		( * e -- ?_n-1 ... ?_1 ?_0 e )	the stack is restored to the state from the corresponding catch

	The implementation hangs on the definitions of catch and throw and the state shared
	between them.

	Up to this point, the return stack has consisted merely of a list of return addresses,
	with the top of the return stack being the return address where we will resume executing
	when the current word EXITs.  However catch will push a more complicated 'exception stack
	frame' on the return stack.  The exception stack frame records some things about the
	state of execution at the time that catch was called.

	When called, throw walks up the return stack (the process is called 'unwinding') until
	it finds the exception stack frame.  It then uses the data in the exception stack frame
	to restore the state allowing execution to continue after the matching catch.  (If it
	unwinds the stack and doesn't find the exception stack frame then it prints a message
	and drops back to the prompt, which is also normal behaviour for so-called 'uncaught
	exceptions').

	This is what the exception stack frame looks like.  (As is conventional, the return stack
	is shown growing downwards from higher to lower memory addresses).

		+------------------------------+
		| return address from catch    |   Notice this is already on the
		|                              |   return stack when catch is called.
		+------------------------------+
		| original parameter stack     |
		| pointer                      |
		+------------------------------+  ^
		| exception stack marker       |  |
		| (exception-marker)           |  |   Direction of stack
		+------------------------------+  |   unwinding by throw.
						  |
						  |

	The exception-marker marks the entry as being an exception stack frame rather than an
	ordinary return address, and it is this which throw "notices" as it is unwinding the
	stack.  (If you want to implement more advanced exceptions such as TRY...WITH then
	you'll need to use a different value of marker if you want the old and new exception stack
	frame layouts to coexist).

	What happens if the executed word doesn't throw an exception?  It will eventually
	return and call exception-marker, so exception-marker had better do something sensible
	without us needing to modify exit.  This nicely gives us a suitable definition of
	exception-marker, namely a function that just drops the stack frame and itself
	returns (thus "returning" from the original catch).

	One thing to take from this is that exceptions are a relatively lightweight mechanism
	in FORTH.
)

: exception-marker
	rdrop			( drop the original parameter stack pointer )
	0			( there was no exception, this is the normal return path )
;

: catch		( xt -- exn? )
	dsp@ 4+ >r		( save parameter stack pointer (+4 because of xt) on the return stack )
	' exception-marker 4+	( push the address of the rdrop inside exception-marker ... )
	>r			( ... on to the return stack so it acts like a return address )
	execute			( execute the nested function )
;

: throw		( n -- )
	?dup if			( only act if the exception code <> 0 )
		rsp@ 			( get return stack pointer )
		begin
			dup r0 4- <		( RSP < r0 )
		while
			dup @			( get the return stack entry )
			' exception-marker 4+ = if	( found the exception-marker on the return stack )
				4+			( skip the exception-marker on the return stack )
				rsp!			( restore the return stack pointer )

				( Restore the parameter stack. )
				dup dup dup		( reserve some working space so the stack for this word
							  doesn't coincide with the part of the stack being restored )
				r>			( get the saved parameter stack pointer | n dsp )
				4-			( reserve space on the stack to store n )
				swap over		( dsp n dsp )
				!			( write n on the stack )
				dsp! exit		( restore the parameter stack pointer, immediately exit )
			then
			4+
		repeat

		( No matching catch - print a message and restart the INTERPRETer. )
		drop

		case
		0 1- of	( abort )
			." ABORTED" cr
		endOf
			( default case )
			." UNCAUGHT throw "
			dup . cr
		endCase
		quit
	then
;

: abort		( -- )
	0 1- throw
;

( Print a stack trace by walking up the return stack. )
: print-stack-trace
	rsp@				( start at caller of this function )
	begin
		dup r0 4- <		( RSP < r0 )
	while
		dup @			( get the return stack entry )
		case
		' exception-marker 4+ of	( is it the exception stack frame? )
			." catch ( DSP="
			4+ dup @ u.		( print saved stack pointer )
			." ) "
		endOf
						( default case )
			dup
			cfa>			( look up the codeword to get the dictionary entry )
			?dup if			( and print it )
				2dup			( dea addr dea )
				id.			( print word from dictionary entry )
				[ char + ] literal emit
				swap >dfa 4+ - .	( print offset )
			then
		endCase
		4+			( move up the stack )
	repeat
	drop
	cr
;

(
	C STRINGS ----------------------------------------------------------------------

	FORTH strings are represented by a start address and length kept on the stack or in memory.

	Most FORTHs don't handle C strings, but we need them in order to access the process arguments
	and environment left on the stack by the Linux kernel, and to make some system calls.

	Operation	Input		Output		FORTH word	Notes
	----------------------------------------------------------------------

	Create FORTH string		addr len	s" ..."

	Create C string			c-addr		z" ..."

	C -> FORTH	c-addr		addr len	dup strLen

	FORTH -> C	addr len	c-addr		cString		Allocated in a temporary buffer, so
									should be consumed / copied immediately.
									FORTH string should not contain NULs.

	For example, dup strLen tell prints a C string.
)

(
	z" .." is like s" ..." except that the string is terminated by an ASCII NUL character.

	To make it more like a C string, at runtime z" just leaves the address of the string
	on the stack (not address & length as with S").  To implement this we need to add the
	extra NUL to the string and also a drop instruction afterwards.  Apart from that the
	implementation just a modified S".
)
: z" immediate
	state @ if	( compiling? )
		' litstring ,	( compile litstring )
		here @		( save the address of the length word on the stack )
		0 ,		( dummy length - we don't know what it is yet )
		begin
			key 		( get next character of the string )
			dup '"' <>
		while
			here @ c!	( store the character in the compiled image )
			1 here +!	( increment here pointer by 1 byte )
		repeat
		0 here @ c!	( add the ASCII NUL byte )
		1 here +!
		drop		( drop the double quote character at the end )
		dup		( get the saved address of the length word )
		here @ swap -	( calculate the length )
		4-		( subtract 4 (because we measured from the start of the length word) )
		swap !		( and back-fill the length location )
		align		( round up to next multiple of 4 bytes for the remaining code )
		' drop ,	( compile drop (to drop the length) )
	else		( immediate mode )
		here @		( get the start address of the temporary space )
		begin
			key
			dup '"' <>
		while
			over c!		( save next character )
			1+		( increment address )
		repeat
		drop		( drop the final " character )
		0 swap c!	( store final ASCII NUL )
		here @		( push the start address )
	then
;

: strLen 	( str -- len )
	dup		( save start address )
	begin
		dup c@ 0<>	( zero byte found? )
	while
		1+
	repeat

	swap -		( calculate the length )
;

: cString	( addr len -- c-addr )
	swap over	( len saddr len )
	here @ swap	( len saddr daddr len )
	cmove		( len )

	here @ +	( daddr+len )
	0 swap c!	( store terminating NUL char )

	here @ 		( push start address )
;
