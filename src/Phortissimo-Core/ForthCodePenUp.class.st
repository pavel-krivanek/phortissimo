Class {
	#name : #ForthCodePenUp,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenUp >> execute [ 

	forth plotter penUp
]

{ #category : #accessing }
ForthCodePenUp >> name [

	^ 'penUp'
]
