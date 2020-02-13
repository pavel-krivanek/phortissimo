Class {
	#name : #ForthCodeNumber,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Parsing'
}

{ #category : #accessing }
ForthCodeNumber >> execute [ 

	| length wordStringAddress numberErrorAssociation |
	
	length := forth pop asSigned32.
	wordStringAddress := forth pop asUnsigned32.
	
	numberErrorAssociation := forth privNumber: wordStringAddress length: length.
	
	forth push: numberErrorAssociation key asUnsigned4Bytes.
	forth push: numberErrorAssociation value asUnsigned4Bytes. "error character index"
]

{ #category : #accessing }
ForthCodeNumber >> name [

	^ 'number'
]
