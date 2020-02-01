Class {
	#name : #ForthCodeGE,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeGE >> execute [ 

	| a b |
	b := forth pop.
	a := forth pop.
	forth push: (a >= b) asBit
]

{ #category : #accessing }
ForthCodeGE >> name [

	^ '>='
]