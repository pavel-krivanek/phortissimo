Class {
	#name : #ForthCodeNRot,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeNRot >> execute [ 

	| a b c |
	
	a := forth pop.
	b := forth pop.
	c := forth pop.
	forth push: a.
	forth push: c.
	forth push: b.
	
]

{ #category : #accessing }
ForthCodeNRot >> name [

	^ '-rot'
]
