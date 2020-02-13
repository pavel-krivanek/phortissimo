Class {
	#name : #ForthCodeIncr,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeIncr >> execute [ 

	forth push: (forth pop asSigned32 + 1) asUnsigned4Bytes 
]

{ #category : #accessing }
ForthCodeIncr >> name [

	^ '1+'
]
