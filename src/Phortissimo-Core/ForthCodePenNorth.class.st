Class {
	#name : #ForthCodePenNorth,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenNorth >> execute [ 

	forth plotter up
]

{ #category : #accessing }
ForthCodePenNorth >> name [

	^ 'penNorth'
]
