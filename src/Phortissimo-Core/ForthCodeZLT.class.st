Class {
	#name : #ForthCodeZLT,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZLT >> execute [ 

	forth push: (forth pop asSigned32  < 0) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeZLT >> name [

	^ '0<'
]
