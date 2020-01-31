Class {
	#name : #ForthCodeZEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZEqu >> execute [ 

	forth push: (forth pop = 0) asBit
]

{ #category : #accessing }
ForthCodeZEqu >> name [

	^ '0='
]
