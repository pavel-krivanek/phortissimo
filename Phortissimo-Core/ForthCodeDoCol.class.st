Class {
	#name : #ForthCodeDoCol,
	#superclass : #ForthCode,
	#category : #'Phortissimo-Core-Code-Basic'
}

{ #category : #evaluating }
ForthCodeDoCol >> execute [ 

	forth pushToReturnStack: forth pcNext.
	forth pcCurrent: (forth pcCurrent + forth wordSize).
	forth pcNext: forth pcCurrent.
	"forth privNext."
	
]
