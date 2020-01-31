Class {
	#name : #ForthCodeZGE,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZGE >> execute [ 

	forth push: (forth pop >= 0) asBit
]

{ #category : #accessing }
ForthCodeZGE >> name [

	^ '0>='
]
