Class {
	#name : #ForthCodeZGT,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZGT >> execute [ 

	forth push: (forth pop > 0) asBit
]

{ #category : #accessing }
ForthCodeZGT >> name [

	^ '0>'
]
