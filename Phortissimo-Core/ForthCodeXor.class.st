Class {
	#name : #ForthCodeXor,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeXor >> execute [ 

	| a b |
	a := forth pop.
	b := forth pop.
	forth push: (a bitXor: b)
]

{ #category : #accessing }
ForthCodeXor >> name [

	^ 'xor'
]
