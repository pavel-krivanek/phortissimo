Class {
	#name : #ForthCodeFetchByte,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeFetchByte >> execute [ 

	| address |
	address := forth pop.
	forth push: (forth byteAt: address)
]

{ #category : #accessing }
ForthCodeFetchByte >> name [

	^ 'c@'
]
