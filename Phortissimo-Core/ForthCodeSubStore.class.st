Class {
	#name : #ForthCodeSubStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeSubStore >> execute [ 

	| address decrement |
	address := forth pop.
	decrement := forth pop.
	forth writeWord: (forth wordAt: address)-decrement at: address 
	
]

{ #category : #accessing }
ForthCodeSubStore >> name [

	^ '-!'
]
