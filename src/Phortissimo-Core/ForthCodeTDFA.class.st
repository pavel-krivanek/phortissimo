Class {
	#name : #ForthCodeTDFA,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeTDFA >> execute [ 

	| wordAddress |
	
	wordAddress := forth pop asUnsigned32.
	forth push: ((forth codewordOf: wordAddress) + forth wordSize) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeTDFA >> name [

	^ '>dfa'
]
