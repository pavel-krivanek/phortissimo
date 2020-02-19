Class {
	#name : #ForthCodePenSouth,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenSouth >> execute [ 

	forth plotter down
]

{ #category : #accessing }
ForthCodePenSouth >> name [

	^ 'penSouth'
]
