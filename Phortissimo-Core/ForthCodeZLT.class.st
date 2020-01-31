Class {
	#name : #ForthCodeZLT,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZLT >> execute [ 

	forth push: (forth pop < 0) asBit
]

{ #category : #accessing }
ForthCodeZLT >> name [

	^ '0<'
]
