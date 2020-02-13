Class {
	#name : #ForthCodeHalt,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Interpret'
}

{ #category : #accessing }
ForthCodeHalt >> execute [ 

	self halt
]

{ #category : #accessing }
ForthCodeHalt >> name [

	^ 'halt'
]
