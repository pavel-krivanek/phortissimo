Class {
	#name : #ForthCodeRot,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeRot >> execute [ 

	| a b c |
	
	a := forth pop.
	b := forth pop.
	c := forth pop.
	forth push: b.
	forth push: a.
	forth push: c.
	
]

{ #category : #accessing }
ForthCodeRot >> name [

	^ 'rot'
]
