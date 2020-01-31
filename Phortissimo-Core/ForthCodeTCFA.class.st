Class {
	#name : #ForthCodeTCFA,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeTCFA >> execute [ 

	| wordAddress |
	
	wordAddress := forth pop.
	forth push: (forth codewordOf: wordAddress)
]

{ #category : #accessing }
ForthCodeTCFA >> name [

	^ '>cfa'
]
