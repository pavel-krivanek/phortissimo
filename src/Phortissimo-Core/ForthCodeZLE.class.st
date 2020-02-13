Class {
	#name : #ForthCodeZLE,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZLE >> execute [ 

	forth push: (forth pop asSigned32 <= 0) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeZLE >> name [

	^ '0<='
]
