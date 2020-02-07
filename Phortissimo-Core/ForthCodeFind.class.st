Class {
	#name : #ForthCodeFind,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeFind >> execute [ 

	| length address toFind |
	
	length := forth pop.
	address := forth pop.
	toFind := forth memory copyFrom: address to: address + length - 1.
	forth push: (forth find: toFind)
	
]

{ #category : #accessing }
ForthCodeFind >> name [

	^ 'find'
]
