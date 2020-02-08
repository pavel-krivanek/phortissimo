Class {
	#name : #ForthCodeXor,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeXor >> execute [ 

	| a b |
	a := forth pop asSigned32.
	b := forth pop asSigned32.
	forth push: (a bitXor: b) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeXor >> name [

	^ 'xor'
]
