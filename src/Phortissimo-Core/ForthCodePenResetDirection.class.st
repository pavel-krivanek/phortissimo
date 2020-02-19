Class {
	#name : #ForthCodePenResetDirection,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenResetDirection >> execute [ 

	forth plotter resetDirection
]

{ #category : #accessing }
ForthCodePenResetDirection >> name [

	^ 'penReset'
]
