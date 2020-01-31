Class {
	#name : #ForthCodeStoreByte,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeStoreByte >> execute [ 

	| address value |
	address := forth pop.
	value := forth pop.
	forth writeByte: (value bitAnd: 16rFF) at: address
]

{ #category : #accessing }
ForthCodeStoreByte >> name [

	^ 'c!'
]
