Class {
	#name : #ForthCodeTDFA,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeTDFA >> execute [ 

	| wordAddress current length |
	
	wordAddress := forth pop.
	current := wordAddress + forth wordSize.
	length := forth lengthByteAt: current.
	
	forth push: current + 1 + length + forth wordSize
]

{ #category : #accessing }
ForthCodeTDFA >> name [

	^ '>dfa'
]
