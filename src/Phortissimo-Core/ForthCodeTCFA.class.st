Class {
	#name : #ForthCodeTCFA,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeTCFA >> execute [ 

	| wordAddress |
	
	wordAddress := forth pop asUnsigned32.
	forth push: (forth codewordOf: wordAddress) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeTCFA >> name [

	^ '>cfa'
]
