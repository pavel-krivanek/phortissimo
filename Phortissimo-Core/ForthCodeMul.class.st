Class {
	#name : #ForthCodeMul,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeMul >> execute [ 

	forth push: (forth pop asSigned32 * forth pop asSigned32) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeMul >> name [

	^ '*'
]
