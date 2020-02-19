Class {
	#name : #ForthCodePenLeft,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenLeft >> execute [ 

	forth plotter left
]

{ #category : #accessing }
ForthCodePenLeft >> name [

	^ 'penLeft'
]
