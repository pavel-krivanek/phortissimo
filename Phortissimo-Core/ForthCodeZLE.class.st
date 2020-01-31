Class {
	#name : #ForthCodeZLE,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZLE >> execute [ 

	forth push: (forth pop <= 0) asBit
]

{ #category : #accessing }
ForthCodeZLE >> name [

	^ '0<='
]
