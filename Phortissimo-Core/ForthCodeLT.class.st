Class {
	#name : #ForthCodeLT,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeLT >> execute [ 

	| a b |
	b := forth pop.
	a := forth pop.
	forth push: (a < b) asBit
]

{ #category : #accessing }
ForthCodeLT >> name [

	^ '<'
]
