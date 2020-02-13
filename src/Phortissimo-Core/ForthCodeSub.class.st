Class {
	#name : #ForthCodeSub,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeSub >> execute [ 

	| a |
	a := forth pop asSigned32.
	forth push: (forth pop asSigned32 - a) asUnsigned4Bytes.	
]

{ #category : #accessing }
ForthCodeSub >> name [

	^ '-'
]
