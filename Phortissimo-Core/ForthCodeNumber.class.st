Class {
	#name : #ForthCodeNumber,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Parsing'
}

{ #category : #accessing }
ForthCodeNumber >> execute [ 

	| length wordStringAddress numberErrorAssociation |
	
	length := forth pop.
	wordStringAddress := forth pop.
	
	numberErrorAssociation := forth privNumber: wordStringAddress length: length.
	
	forth push: numberErrorAssociation key.
	forth push: numberErrorAssociation value. "error character index"
]

{ #category : #accessing }
ForthCodeNumber >> name [

	^ 'number'
]
