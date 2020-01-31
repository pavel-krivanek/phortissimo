Class {
	#name : #ForthTestComparisonPrimitives,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestComparisonPrimitives >> testEqu [ 

	| code |
	code := (ForthCodeEqu on: forth).
	code execute.
	
	self assertStack: #(1 2 3 0).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testGE [

	| code |
	code := (ForthCodeGE on: forth).
	code execute.
	
	self assertStack: #(1 2 3 0).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testGT [

	| code |
	code := (ForthCodeGT on: forth).
	code execute.
	
	self assertStack: #(1 2 3 0).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testLE [

	| code |
	code := (ForthCodeLE on: forth).
	code execute.
	
	self assertStack: #(1 2 3 1).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testLT [

	| code |
	code := (ForthCodeLT on: forth).
	code execute.
	
	self assertStack: #(1 2 3 1).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testNEqu [ 

	| code |
	code := (ForthCodeNEqu on: forth).
	code execute.
	
	self assertStack: #(1 2 3 1).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testZEqu [

	| code |
	code := (ForthCodeZEqu on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 0).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testZGE [

	| code |
	code := (ForthCodeZGE on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 1).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testZGT [

	| code |
	code := (ForthCodeZGT on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 1).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testZLE [

	| code |
	code := (ForthCodeZLE on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 0).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testZLT [

	| code |
	code := (ForthCodeZLT on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 0).

]

{ #category : #tests }
ForthTestComparisonPrimitives >> testZNEqu [

	| code |
	code := (ForthCodeZNEqu on: forth).
	code execute.
	
	self assertStack: #(1 2 3 4 1).

]
