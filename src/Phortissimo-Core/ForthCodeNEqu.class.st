Class {
	#name : #ForthCodeNEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeNEqu >> execute [ 

	| a b |
	b := forth pop asSigned32.
	a := forth pop asSigned32.
	forth push: (a = b ifTrue: [ 0 ] ifFalse: [ 1 ]) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeNEqu >> name [

	^ '<>'
]
