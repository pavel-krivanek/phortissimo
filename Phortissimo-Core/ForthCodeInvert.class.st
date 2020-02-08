Class {
	#name : #ForthCodeInvert,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Bitwise primitives'
}

{ #category : #accessing }
ForthCodeInvert >> execute [ 

	forth push: forth pop asUnsigned32 bitInvert asUnsigned4Bytes.
]

{ #category : #accessing }
ForthCodeInvert >> name [

	^ 'invert'
]
