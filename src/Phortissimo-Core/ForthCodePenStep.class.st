Class {
	#name : #ForthCodePenStep,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenStep >> execute [ 

	forth plotter step
]

{ #category : #accessing }
ForthCodePenStep >> name [

	^ 'penStep'
]
