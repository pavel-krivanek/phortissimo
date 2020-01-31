Class {
	#name : #ForthCodeChar,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeChar >> execute [ 

	| addressLengthAssociation |
	
	addressLengthAssociation := forth privWord.
	forth push: (forth byteAt: addressLengthAssociation key ).
]

{ #category : #accessing }
ForthCodeChar >> name [

	^ 'char'
]
