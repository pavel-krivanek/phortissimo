Class {
	#name : #ForthCodePenDown,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenDown >> execute [ 

	forth plotter penDown
]

{ #category : #accessing }
ForthCodePenDown >> name [

	^ 'penDown'
]
