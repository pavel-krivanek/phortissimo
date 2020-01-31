Class {
	#name : #ForthCodeWithHead,
	#superclass : #ForthCode,
	#category : #'Phortissimo-Core-Code-Basic'
}

{ #category : #accessing }
ForthCodeWithHead >> codewordFor: position [

	^ position + forth wordSize
]

{ #category : #accessing }
ForthCodeWithHead >> finishAt: originalPosition [

	| newPosition |
	
	newPosition := originalPosition + 1.
	forth addLabel: 'next_', self label address: newPosition.
	forth writeCode: (ForthCodeNext on: forth) at: newPosition.
	
	^ newPosition
]

{ #category : #accessing }
ForthCodeWithHead >> flags [ 

	^ 0
]

{ #category : #accessing }
ForthCodeWithHead >> installAt: initialPosition [

	| position headAddress |
		
	position := initialPosition.
	headAddress := position.
	
	forth addLabel: 'name_', self label address: position.
	forth writeWord: forth lastWord at: position.
	position := position + forth wordSize.
	
	forth writeByte: self name size + self flags at: position.
	position := position + 1.
	
	self name withIndexDo: [ :char :i | forth writeByte: (self name at: i) asciiValue at: position + i - 1 ].
	position := position + self name size.

	forth addLabel:  self label address: position.

	forth addLabel: 'codeword_', self label address: position.
	forth writeWord: (self codewordFor: position) at: position.
	position := position + forth wordSize.

	forth addLabel: 'code_', self label address: position.
	position := self writeCodeAt: position.
	
	position := self finishAt: position.
	
	forth lastWord: headAddress.

	^ position.
	
	
	
]

{ #category : #accessing }
ForthCodeWithHead >> name [ 

	^ self subclassResponsibility
]

{ #category : #accessing }
ForthCodeWithHead >> writeCodeAt: originalPosition [ 

	forth addLabel: 'code_', self label address: originalPosition.
	forth writeCode: self at: originalPosition.
	^ originalPosition
]
