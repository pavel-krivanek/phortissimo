Class {
	#name : #ForthCodeDivMod,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDivMod >> execute [ 

	| a b |
	b := forth pop.
	a := forth pop.
	forth push: a \\ b.
	forth push: a // b.
]

{ #category : #accessing }
ForthCodeDivMod >> name [

	^ '/mod'
]
