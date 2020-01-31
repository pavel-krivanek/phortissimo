Class {
	#name : #ForthCodeWithHeadCompiled,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic'
}

{ #category : #accessing }
ForthCodeWithHeadCompiled >> codewordFor: position [

	^ (forth addressForLabel: 'DOCOL') 
]

{ #category : #accessing }
ForthCodeWithHeadCompiled >> codewordLabels [

	self subclassResponsibility 
]

{ #category : #accessing }
ForthCodeWithHeadCompiled >> execute [ 

	"nothing here"
]

{ #category : #accessing }
ForthCodeWithHeadCompiled >> finishAt: originalPosition [

	^ originalPosition
]

{ #category : #accessing }
ForthCodeWithHeadCompiled >> writeCodeAt: originalPosition [ 

	| position | 
	
	position := originalPosition.
	
	self codewordLabels do: [ :labelOrNumber |
		| aValue |
		labelOrNumber isNumber
			ifTrue: [ 
				forth writeWordSigned: labelOrNumber at: position. ]
			ifFalse: [ 
				aValue := forth addressForLabel: labelOrNumber inFutureSet: position.
				forth writeWord: aValue at: position. ].
		
		position := position + forth wordSize ].
	
	^ position
]
