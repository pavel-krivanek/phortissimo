Class {
	#name : #ForthCodeAddStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeAddStore >> execute [ 

	| address increment |
	address := forth pop asUnsigned32.
	increment := forth pop asSigned32.
	forth writeWord: (forth signedWordAt: address)+increment at: address 
	
]

{ #category : #accessing }
ForthCodeAddStore >> name [

	^ '+!'
]
