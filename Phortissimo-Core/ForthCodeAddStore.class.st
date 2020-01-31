Class {
	#name : #ForthCodeAddStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeAddStore >> execute [ 

	| address increment |
	address := forth pop.
	increment := forth pop.
	forth writeWord: (forth wordAt: address)+increment at: address 
	
]

{ #category : #accessing }
ForthCodeAddStore >> name [

	^ '+!'
]
