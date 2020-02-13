Class {
	#name : #ForthTestBitwisePrimitives,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestBitwisePrimitives >> testAnd [

	| code |
	code := (ForthCodeAnd on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 ).



]

{ #category : #tests }
ForthTestBitwisePrimitives >> testInvert [

	| code |
	code := (ForthCodeInvert on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 -6).

]

{ #category : #tests }
ForthTestBitwisePrimitives >> testOr [

	| code |
	code := (ForthCodeOr on: forth).
	code execute.
	
	self assertStack: #(1 2 3 5).

]

{ #category : #tests }
ForthTestBitwisePrimitives >> testXor [

	| code |
	code := (ForthCodeXor on: forth).
	code execute.
	
	self assertStack: #(1 2 3 1).
]
