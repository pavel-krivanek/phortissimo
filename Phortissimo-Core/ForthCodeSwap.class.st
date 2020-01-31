Class {
	#name : #ForthCodeSwap,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeSwap >> execute [ 

	| a b |
	
	a := forth pop.	
	b := forth pop.
	forth push: a.
	forth push: b.
]

{ #category : #accessing }
ForthCodeSwap >> name [

	^ 'swap'
]
