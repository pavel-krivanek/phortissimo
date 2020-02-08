Class {
	#name : #ForthCodeExit,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Exit'
}

{ #category : #accessing }
ForthCodeExit >> execute [ 

	forth pcNext: (forth returnStack removeLast: 4) asUnsigned32.

	
]

{ #category : #accessing }
ForthCodeExit >> name [

	^ 'exit'
]
