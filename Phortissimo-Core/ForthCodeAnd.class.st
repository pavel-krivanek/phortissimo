Class {
	#name : #ForthCodeAnd,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeAnd >> execute [ 

	| a b |
	a := forth pop.
	b := forth pop.
	forth push: (a bitAnd: b)
]

{ #category : #accessing }
ForthCodeAnd >> name [

	^ 'and'
]
