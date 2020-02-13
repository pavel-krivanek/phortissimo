Class {
	#name : #ForthCodeDecr4,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDecr4 >> execute [ 

	forth push: ((forth pop asSigned32) - 4) asUnsigned4Bytes 

]

{ #category : #accessing }
ForthCodeDecr4 >> name [

	^ '4-'
]
