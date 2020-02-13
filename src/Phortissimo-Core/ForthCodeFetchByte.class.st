Class {
	#name : #ForthCodeFetchByte,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeFetchByte >> execute [ 

	| address |
	address := forth pop asSigned32.
	forth push: (forth byteAt: address) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeFetchByte >> name [

	^ 'c@'
]
