Class {
	#name : #ForthCodeLitString,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-String literals'
}

{ #category : #accessing }
ForthCodeLitString >> execute [ 

	self shouldBeImplemented 
]

{ #category : #accessing }
ForthCodeLitString >> name [

	^ 'litstring'
]
