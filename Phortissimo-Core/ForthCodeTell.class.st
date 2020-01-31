Class {
	#name : #ForthCodeTell,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-String literals'
}

{ #category : #accessing }
ForthCodeTell >> execute [ 

	| length address text |
	
	length := forth pop.
	address := forth pop.
	text := forth memory copyFrom: address to: address+length-1.
	forth outputBuffer addAllLast: text.
]

{ #category : #accessing }
ForthCodeTell >> name [

	^ 'tell'
]
