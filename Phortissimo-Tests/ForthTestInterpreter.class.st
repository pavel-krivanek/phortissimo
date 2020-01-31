Class {
	#name : #ForthTestInterpreter,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #running }
ForthTestInterpreter >> setUp [

	forth := Forth new.
	forth memory.

	forth pc: 1. "docol" 
	forth pcCurrent: (forth addressForLabel: 'codeword_QUIT').
	forth pcNext: forth pcCurrent + forth wordSize.

	forth varStateValue: 0.

]

{ #category : #running }
ForthTestInterpreter >> testColon [

	forth push: 1.
	forth << ': test dup ; test'.
	forth run.
	self assertStack: #(1 1).
	
]

{ #category : #running }
ForthTestInterpreter >> testCompileNumber [

	forth << ': test 1 ; test'.
	forth run.
	self assertStack: #(1).
	
]

{ #category : #running }
ForthTestInterpreter >> testDrop [

	forth << 'drop'.
	forth push: 1.
	forth run.
	self assertStack: #().
]

{ #category : #running }
ForthTestInterpreter >> testDup [ 

	forth << ' dup'.
	forth push: 1.
	forth run.
	self assertStack: #(1 1).
]

{ #category : #running }
ForthTestInterpreter >> testDupWithSpaceAfter [

	forth << ' dup '.
	forth push: 1.
	forth run.
	self assertStack: #(1 1).
]

{ #category : #running }
ForthTestInterpreter >> testDupWithSpaceBefore [ 

	forth << ' dup'.
	forth push: 1.
	forth run.
	self assertStack: #(1 1).
]

{ #category : #running }
ForthTestInterpreter >> testDupWithSpaceOnBothSides [

	forth << ' dup '.
	forth push: 1.
	forth run.
	self assertStack: #(1 1).
]

{ #category : #running }
ForthTestInterpreter >> testNumberLiteral [

	forth << '1'.
	forth run.
	self assertStack: #(1).
]

{ #category : #running }
ForthTestInterpreter >> testPlus [

	forth << '1 2 +'.
	forth run.
	self assertStack: #(3).
]

{ #category : #running }
ForthTestInterpreter >> testPlusWithPreparedStack [

	forth << '+'.
	forth push: 1.
	forth push: 2.
	forth run.
	self assertStack: #(3).
]

{ #category : #running }
ForthTestInterpreter >> testTwoDups [

	forth << 'dup dup'.
	forth push: 1.
	forth run.
	self assertStack: #(1 1 1).
]
