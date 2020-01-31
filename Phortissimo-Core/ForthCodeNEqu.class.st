Class {
	#name : #ForthCodeNEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeNEqu >> execute [ 

	| a b |
	b := forth pop.
	a := forth pop.
	forth push: (a = b ifTrue: [ 0 ] ifFalse: [ 1 ])
]

{ #category : #accessing }
ForthCodeNEqu >> name [

	^ '<>'
]
