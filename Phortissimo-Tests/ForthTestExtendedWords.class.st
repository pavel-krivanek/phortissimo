Class {
	#name : #ForthTestExtendedWords,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

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
ForthTestExtendedWords >> testDiv [

	forth << ': / /mod swap drop ;
7  3 /'.
	
	forth run.
	self assertStack: #(2)
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
ForthTestExtendedWords >> testTick [

	forth << ': test '' drop ;
	test'.
	
	forth run.
	self assertStack: #(12).

]

{ #category : #running }
ForthTestExtendedWords >> testWhileRepeat [

	forth << '
	: while immediate '' %branch , here @ 0 , ;
	: repeat immediate '' branch , swap here @ - , dup here @ swap - swap ! ;
	: 
	: test 10 begin dup 1- dup 0= if exit then again ; 
	test
	'.
	
	forth run.
	self assertStack: #(10 9 8 7 6 5 4 3 2 1 0).

]
