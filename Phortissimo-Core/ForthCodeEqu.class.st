Class {
	#name : #ForthCodeEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeEqu >> execute [ 

	| a b |
	a := forth pop.
	b := forth pop.
	forth push: (a = b) asBit
]

{ #category : #accessing }
ForthCodeEqu >> name [

	^ '='
]
