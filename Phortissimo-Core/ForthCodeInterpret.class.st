Class {
	#name : #ForthCodeInterpret,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Interpret'
}

{ #category : #accessing }
ForthCodeInterpret >> execute [ 

	| addressLengthAssociation address length toFind resultOfFind interpretIsLit numberErrorAssociation executeImmediate aCodeword |
	
	interpretIsLit := false.
	executeImmediate := false.
	addressLengthAssociation := forth privWord.
	address := addressLengthAssociation key.
	length := addressLengthAssociation value.
	
	length = 0 ifTrue: [ 
		"empty input"
		^ self].	
	
	toFind := forth memory copyFrom: address to: address + length - 1.
	resultOfFind := forth find: toFind.
		
	resultOfFind = 0 
		ifFalse: [ 
			aCodeword := forth codewordOf: resultOfFind.
			forth pcCurrent: aCodeword.
			(forth isImmediate: resultOfFind) 
				ifTrue: [ executeImmediate := true ]
		] ifTrue: [
			"1:"
			"Not in the dictionary (not a word) so assume it's a literal number."
				interpretIsLit := true.
			numberErrorAssociation := forth privNumber: address length: length.
			numberErrorAssociation value = 0
				ifTrue: [ 
					interpretIsLit := true.
					aCodeword := (forth addressForLabel: 'codeword_LIT'). ]
				ifFalse: [ self error: 'unknown word: ', (toFind collect: [ :c | Character value: c ] as: String) ].				
		 ].
"2:"	
	(forth varStateValue = 0 or: [executeImmediate])
		ifFalse: [
			
			"compiling"
			forth privComma: aCodeword. "TODO"
			interpretIsLit ifTrue: [ 
				forth privComma: numberErrorAssociation key ].
			forth privNext ]
		ifTrue: [
"4:"
			interpretIsLit 
				ifFalse: [ "execute"
					"forth pcCurrent: aCodeword."
					forth pc: (forth wordAt: aCodeword) - 1 ]
				ifTrue: [ 
"5:"
					forth push: numberErrorAssociation key.
					forth privNext ] 
			 ]
	

	
	
]

{ #category : #accessing }
ForthCodeInterpret >> name [

	^ 'interpret'
]
