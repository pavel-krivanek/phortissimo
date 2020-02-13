Class {
	#name : #ForthCodeZGE,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZGE >> execute [ 

	forth push: (forth pop asSigned32 >= 0) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeZGE >> name [

	^ '0>='
]
