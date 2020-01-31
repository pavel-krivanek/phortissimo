Class {
	#name : #ForthCodeOr,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeOr >> execute [ 

	| a b |
	a := forth pop.
	b := forth pop.
	forth push: (a bitOr: b)
]

{ #category : #accessing }
ForthCodeOr >> name [

	^ 'or'
]
