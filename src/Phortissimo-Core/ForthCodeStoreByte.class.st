Class {
	#name : #ForthCodeStoreByte,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Memory'
}

{ #category : #accessing }
ForthCodeStoreByte >> execute [ 

	| address value |
	address := forth pop asSigned32.
	value := forth pop asSigned32.
	forth writeByte: (value bitAnd: 16rFF) at: address
]

{ #category : #accessing }
ForthCodeStoreByte >> name [

	^ 'c!'
]
