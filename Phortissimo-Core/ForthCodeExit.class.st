Class {
	#name : #ForthCodeExit,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Exit'
}

{ #category : #accessing }
ForthCodeExit >> execute [ 

	forth pcNext: ( forth returnStack removeLast).

	
]

{ #category : #accessing }
ForthCodeExit >> name [

	^ 'exit'
]
