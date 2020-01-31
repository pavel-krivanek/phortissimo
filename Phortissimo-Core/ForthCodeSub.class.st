Class {
	#name : #ForthCodeSub,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeSub >> execute [ 

	| a |
	a := forth pop.
	forth push: (forth pop - a).	
]

{ #category : #accessing }
ForthCodeSub >> name [

	^ '-'
]
