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
	self forth isRunning ifFalse: [ ^ self ].
	forth push: addressLengthAssociation key asUnsigned4Bytes.
	forth push: addressLengthAssociation value asUnsigned4Bytes.
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
