Class {
	#name : #ForthCodePenHome,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenHome >> execute [ 

	forth plotter home
]

{ #category : #accessing }
ForthCodePenHome >> name [

	^ 'penHome'
]
