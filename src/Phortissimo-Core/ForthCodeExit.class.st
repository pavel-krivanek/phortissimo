Class {
	#name : #ForthCodeExit,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Exit'
}

{ #category : #accessing }
ForthCodeExit >> execute [ 

	forth pcNext: (forth popFromReturnStack) asUnsigned32.

	
]

{ #category : #accessing }
ForthCodeExit >> name [

	^ 'exit'
]
