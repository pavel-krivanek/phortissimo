Class {
	#name : #ForthCodeWord,
	#superclass : #ForthCodeWithHead,
	#instVars : [
		'bufferAddress'
	],
	#category : #'Phortissimo-Core-Code-Parsing'
}

{ #category : #accessing }
ForthCodeWord >> execute [ 

	| addressLengthAssociation |
	
	addressLengthAssociation := forth privWord.
	forth push: addressLengthAssociation key.
	forth push: addressLengthAssociation value.
	
]

{ #category : #accessing }
ForthCodeWord >> finishAt: initialPosition [
		
	| newPosition |
	newPosition := super finishAt: initialPosition.
	
	newPosition := newPosition + 1.
	forth addLabel: 'word_buffer' address: newPosition.
	bufferAddress := newPosition.
	
	^ newPosition + forth wordBufferSize
]

{ #category : #accessing }
ForthCodeWord >> name [

	^ 'word'
]
