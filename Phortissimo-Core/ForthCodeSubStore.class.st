Class {
	#name : #ForthCodeSubStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeSubStore >> execute [ 

	| address decrement |
	address := forth pop asUnsigned32.
	decrement := forth pop asSigned32.
	forth writeWord: (forth signedWordAt: address)-decrement at: address 
	
]

{ #category : #accessing }
ForthCodeSubStore >> name [

	^ '-!'
]
