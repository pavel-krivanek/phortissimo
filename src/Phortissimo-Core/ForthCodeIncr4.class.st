Class {
	#name : #ForthCodeIncr4,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeIncr4 >> execute [ 

	forth push: (forth pop asSigned32 + 4) asUnsigned4Bytes 
]

{ #category : #accessing }
ForthCodeIncr4 >> name [

	^ '4+'
]
