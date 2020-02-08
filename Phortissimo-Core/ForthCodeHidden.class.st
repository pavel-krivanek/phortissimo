Class {
	#name : #ForthCodeHidden,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiler extending'
}

{ #category : #accessing }
ForthCodeHidden >> execute [ 

	| entry |
	
	entry := forth pop asUnsigned32.
	forth toggleFlag: forth flagHidden of: entry
]

{ #category : #accessing }
ForthCodeHidden >> name [

	^ 'hidden'
]
