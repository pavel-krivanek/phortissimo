Class {
	#name : #ForthCodeDecr,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDecr >> execute [ 

	forth push: (forth pop asSigned32 - 1) asUnsigned4Bytes 
]

{ #category : #accessing }
ForthCodeDecr >> name [

	^ '1-'
]
