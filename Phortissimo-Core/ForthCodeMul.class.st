Class {
	#name : #ForthCodeMul,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeMul >> execute [ 

	forth push: (forth pop * forth pop)
]

{ #category : #accessing }
ForthCodeMul >> name [

	^ '*'
]
