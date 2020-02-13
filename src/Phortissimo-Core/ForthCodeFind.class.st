Class {
	#name : #ForthCodeFind,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeFind >> execute [ 

	| length address toFind |
	
	length := forth pop asSigned32.
	address := forth pop asUnsigned32.
	toFind := forth memoryCopyFrom: address to: address + length - 1.
	forth push: (forth find: toFind) asUnsigned4Bytes
	
]

{ #category : #accessing }
ForthCodeFind >> name [

	^ 'find'
]
