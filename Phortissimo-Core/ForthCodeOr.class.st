Class {
	#name : #ForthCodeOr,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeOr >> execute [ 

	| a b |
	a := forth pop asSigned32.
	b := forth pop asSigned32.
	forth push: (a bitOr: b) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeOr >> name [

	^ 'or'
]
