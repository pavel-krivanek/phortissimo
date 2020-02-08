Class {
	#name : #ForthCodeNRot,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeNRot >> execute [ 

	| a b c |
	
	a := forth pop asSigned32.
	b := forth pop asSigned32.
	c := forth pop asSigned32.
	forth push: a asUnsigned4Bytes.
	forth push: c asUnsigned4Bytes.
	forth push: b asUnsigned4Bytes.
	
]

{ #category : #accessing }
ForthCodeNRot >> name [

	^ '-rot'
]
