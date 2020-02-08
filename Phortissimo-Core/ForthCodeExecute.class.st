Class {
	#name : #ForthCodeExecute,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Interpret'
}

{ #category : #accessing }
ForthCodeExecute >> execute [ 

	forth pcCurrent: forth pop asUnsigned32.
	forth pc: (forth wordAt: forth pcCurrent) - 1.

]

{ #category : #accessing }
ForthCodeExecute >> finishAt: originalPosition [

	| newPosition |
	
	newPosition := originalPosition + 1.	
	^ newPosition
]

{ #category : #accessing }
ForthCodeExecute >> name [

	^ 'execute'
]
