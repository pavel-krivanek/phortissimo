Class {
	#name : #ForthCodeEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeEqu >> execute [ 

	| a b |
	a := forth pop asSigned32.
	b := forth pop asSigned32.
	forth push: (a = b) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeEqu >> name [

	^ '='
]
