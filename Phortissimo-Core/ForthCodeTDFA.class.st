Class {
	#name : #ForthCodeTDFA,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeTDFA >> execute [ 

	| wordAddress |
	
	wordAddress := forth pop.
	forth push: (forth codewordOf: wordAddress) + forth wordSize 
]

{ #category : #accessing }
ForthCodeTDFA >> name [

	^ '>dfa'
]
