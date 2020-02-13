Class {
	#name : #ForthCodeComma,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiling'
}

{ #category : #accessing }
ForthCodeComma >> execute [ 

	| value |
	value := forth pop asSigned32.
	forth privComma: value.
]

{ #category : #accessing }
ForthCodeComma >> name [

	^ ','
]
