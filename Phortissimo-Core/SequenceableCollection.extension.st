Extension { #name : #SequenceableCollection }

{ #category : #'*Phortissimo-Core' }
SequenceableCollection >> asSigned32 [
		
	^ self asUnsigned32 asSigned32
]

{ #category : #'*Phortissimo-Core' }
SequenceableCollection >> asUnsigned32 [

	| unsigned |
	
	unsigned := ((self at: 4)) +
	  ((self at: 3) <<  8) +
	  ((self at: 2) << 16) +
	  ((self at: 1) << 24).
		
	^ unsigned
]
