Class {
	#name : #ForthCodeDivMod,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDivMod >> execute [ 

	| a b |
	b := forth pop asSigned32.
	a := forth pop asSigned32.
	forth push: (a \\ b) asUnsigned4Bytes.
	forth push: (a // b) asUnsigned4Bytes.
]

{ #category : #accessing }
ForthCodeDivMod >> name [

	^ '/mod'
]
