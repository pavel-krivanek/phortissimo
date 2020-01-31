Class {
	#name : #ForthCodeAdd,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeAdd >> execute [ 

	forth push: (forth pop + forth pop)
]

{ #category : #accessing }
ForthCodeAdd >> name [

	^ '+'
]
