Class {
	#name : #ForthCodeStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeStore >> execute [ 

	| address value |
	address := forth pop asSigned32.
	value := forth pop asSigned32.
	forth writeWord: value at: address 
	
]

{ #category : #accessing }
ForthCodeStore >> name [

	^ '!'
]
