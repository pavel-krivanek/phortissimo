Class {
	#name : #ForthCodeTwoSwap,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoSwap >> execute [ 

	| a b c d |
	
	a := forth pop.	
	b := forth pop.
	c := forth pop.
	d := forth pop.
	forth push: b.
	forth push: a.	
	forth push: d.
	forth push: c.
]

{ #category : #accessing }
ForthCodeTwoSwap >> name [

	^ '2swap'
]
