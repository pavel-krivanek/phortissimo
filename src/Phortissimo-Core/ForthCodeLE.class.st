Class {
	#name : #ForthCodeLE,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeLE >> execute [ 

	| a b |
	b := forth pop asSigned32.
	a := forth pop asSigned32.
	forth push: (a <= b) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeLE >> name [

	^ '<='
]
