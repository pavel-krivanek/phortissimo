Class {
	#name : #ForthCodeFetch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeFetch >> execute [ 

	| address |
	address := forth pop asSigned32.
	forth push: (forth signedWordAt: address) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeFetch >> name [

	^ '@'
]
