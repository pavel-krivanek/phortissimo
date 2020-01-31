Class {
	#name : #ForthCodeStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeStore >> execute [ 

	| address value |
	address := forth pop.
	value := forth pop.
	forth writeWord: value at: address 
	
]

{ #category : #accessing }
ForthCodeStore >> name [

	^ '!'
]
