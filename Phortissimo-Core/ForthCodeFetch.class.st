Class {
	#name : #ForthCodeFetch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeFetch >> execute [ 

	| address |
	address := forth pop.
	forth push: (forth wordAt: address)
]

{ #category : #accessing }
ForthCodeFetch >> name [

	^ '@'
]
