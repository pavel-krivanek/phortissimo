Class {
	#name : #ForthTestInstructions,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #running }
ForthTestInstructions >> setUp [

	forth := Forth new.
	forth memory.

	forth pc: 1. "docol" 
	forth pcCurrent: (forth addressForLabel: 'codeword_QUIT').
	forth pcNext: forth pcCurrent + forth wordSize.

	forth varStateValue: 0.

]

{ #category : #'tests - bitwise primitives' }
ForthTestInstructions >> test0r [

	forth << '5 6 or'.
	forth run.
	self assertStack: #(7).
	

]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testAdd [

	forth << '1 1 2 +'.
	forth run.
	self assertStack: #(1 3).
]

{ #category : #'tests - bitwise primitives' }
ForthTestInstructions >> testAnd [ 

	forth << '5 4 and'.
	forth run.
	self assertStack: #(4).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testDecr [

	forth << '1 3 1-'.
	forth run.
	self assertStack: #(1 2).
]

{ #category : #tests }
ForthTestInstructions >> testDecr4 [

	forth << '1 3 4-'.
	forth run.
	self assertStack: #(1 -1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testDivMod [

	forth << '7 3 /mod'.
	forth run.
	self assertStack: #(1 2).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testEqu [

	forth << '1 2 = 2 2 ='.
	forth run.
	self assertStack: #(0 1).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testGE [

	forth << '1 2 >= 2 2 >= 3 2 >='.
	forth run.
	self assertStack: #(0 1 1).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testGT [

	forth << '1 2 > 2 2 > 3 2 >'.
	forth run.
	self assertStack: #(0 0 1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testIncr [

	forth << '1 2 1+'.
	forth run.
	self assertStack: #(1 3).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testIncr4 [

	forth << '1 2 4+'.
	forth run.
	self assertStack: #(1 6).
]

{ #category : #'tests - bitwise primitives' }
ForthTestInstructions >> testInvert [

	forth << '5 invert -6 invert'.
	forth run.
	self assertStack: #(-6 5).	
	
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testLE [

	forth << '1 2 <= 2 2 <= 3 2 <='.
	forth run.
	self assertStack: #(1 1 0).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testLT [

	forth << '1 2 < 2 2 < 3 2 <'.
	forth run.
	self assertStack: #(1 0 0).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testMul [

	forth << '1 2 3 *'.
	forth run.
	self assertStack: #(1 6).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testNEqu [

	forth << '1 2 <> 2 2 <>'.
	forth run.
	self assertStack: #(1 0).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testNZEqu [

	forth << '0 0<> 1 0<>'.
	forth run.
	self assertStack: #(0 1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testOver [

	forth << '1 2 3 over'.
	forth run.
	self assertStack: #(1 2 3 2).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testQDup [

	forth << '1 2 ?dup 0 ?dup'.
	forth run.
	self assertStack: #(1 2 2 0).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testRot [

	forth << '1 2 3 rot'.
	forth run.
	self assertStack: #(2 3 1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testSub [

	forth << '1 1 2 -'.
	forth run.
	self assertStack: #(1 -1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testSwap [

	forth << '1 2 swap'.
	forth run.
	self assertStack: #(2 1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testTwoDrop [

	forth << '1 2 3 2drop'.
	forth run.
	self assertStack: #(1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testTwoDup [

	forth << '1 2 3 4 2swap'.
	forth run.
	self assertStack: #(3 4 1 2).
]

{ #category : #'tests - bitwise primitives' }
ForthTestInstructions >> testXor [

	forth << '5 6 xor'.
	forth run.
	self assertStack: #(3).
	
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testZEqu [

	forth << '0 0= 1 0='.
	forth run.
	self assertStack: #(1 0).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testZGE [

	forth << '0 0>= 1 0>= -1 0>='.
	forth run.
	self assertStack: #(1 1 0).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testZGT [

	forth << '0 0> 1 0> -1 0>'.
	forth run.
	self assertStack: #(0 1 0).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testZLE [

	forth << '0 0<= 1 0<= -1 0<='.
	forth run.
	self assertStack: #(1 0 1).
]

{ #category : #'tests - comparison primitives' }
ForthTestInstructions >> testZLT [

	forth << '0 0< 1 0< -1 0<'.
	forth run.
	self assertStack: #(0 0 1).
]

{ #category : #'tests - basic primitives' }
ForthTestInstructions >> testdup [

	forth << '1 2 dup'.
	forth run.
	self assertStack: #(1 2 2).
]
