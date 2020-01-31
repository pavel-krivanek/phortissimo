Class {
	#name : #ForthCodeInvert,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeInvert >> execute [ 

	forth push: forth pop bitInvert.
]

{ #category : #accessing }
ForthCodeInvert >> name [

	^ 'invert'
]
