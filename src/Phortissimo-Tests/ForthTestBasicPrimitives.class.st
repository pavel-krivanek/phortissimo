Class {
	#name : #ForthTestBasicPrimitives,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #running }
ForthTestBasicPrimitives >> testAdd [

	| code |
	code := (ForthCodeAdd on: forth).
	code execute.
	
	self assertStack: #(1 2 3 9).

]

{ #category : #running }
ForthTestBasicPrimitives >> testDecr [

	| code |
	code := (ForthCodeDecr on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 4).

]

{ #category : #running }
ForthTestBasicPrimitives >> testDecr4 [

	| code |
	code := (ForthCodeDecr4 on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 1).

]

{ #category : #running }
ForthTestBasicPrimitives >> testDivMod [

	| code |
	code := (ForthCodeDivMod on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 0).

]

{ #category : #running }
ForthTestBasicPrimitives >> testDrop [

	| code |
	code := (ForthCodeDrop on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4).
]

{ #category : #running }
ForthTestBasicPrimitives >> testDup [

	| code |
	code := (ForthCodeDup on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 5 5).
]

{ #category : #running }
ForthTestBasicPrimitives >> testIncr [

	| code |
	code := (ForthCodeIncr on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 6).

]

{ #category : #running }
ForthTestBasicPrimitives >> testIncr4 [

	| code |
	code := (ForthCodeIncr4 on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 9).

]

{ #category : #running }
ForthTestBasicPrimitives >> testMul [

	| code |
	code := (ForthCodeMul on: forth).
	code execute.
	
	self assertStack: #(1 2 3 20).

]

{ #category : #running }
ForthTestBasicPrimitives >> testNRot [

	| code |
	code := (ForthCodeNRot on: forth).
	code execute.
	
	self assertStack: #(1 2 5 3 4).
]

{ #category : #running }
ForthTestBasicPrimitives >> testOver [

	| code |
	code := (ForthCodeOver on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 5 4).
]

{ #category : #running }
ForthTestBasicPrimitives >> testQDup [

	| code |
	code := (ForthCodeQDup on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 5 5).
	
	self resetStack: #(1 2 3 4 5 0).
	code execute.
	self assertStack: #(1 2 3 4 5 0)
	
]

{ #category : #running }
ForthTestBasicPrimitives >> testRot [

	| code |
	code := (ForthCodeRot on: forth).
	code execute.
	
	self assertStack: #(1 2 4 5 3).
]

{ #category : #running }
ForthTestBasicPrimitives >> testSub [

	| code |
	code := (ForthCodeSub on: forth).
	code execute.
	
	self assertStack: #(1 2 3 -1).

]

{ #category : #running }
ForthTestBasicPrimitives >> testSwap [

	| code |
	code := (ForthCodeSwap on: forth).
	code execute.
	
	self assertStack: #(1 2 3 5 4).
]

{ #category : #running }
ForthTestBasicPrimitives >> testTwoDrop [

	| code |
	code := (ForthCodeTwoDrop on: forth).
	code execute.
	
	self assertStack: #(1 2 3).
]

{ #category : #running }
ForthTestBasicPrimitives >> testTwoDup [

	| code |
	code := (ForthCodeTwoDup on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 5 4 5).
]

{ #category : #running }
ForthTestBasicPrimitives >> testTwoSwap [

	| code |
	code := (ForthCodeTwoSwap on: forth).
	code execute.
	
	self assertStack: #(1 4 5 2 3).
]
