Class {
	#name : #ForthCodeZGT,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZGT >> execute [ 

	forth push: (forth pop asSigned32 > 0) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeZGT >> name [

	^ '0>'
]
