Class {
	#name : #ForthCodeAnd,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeAnd >> execute [ 

	| a b |
	a := forth pop asSigned32.
	b := forth pop asSigned32.
	forth push: (a bitAnd: b) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeAnd >> name [

	^ 'and'
]
