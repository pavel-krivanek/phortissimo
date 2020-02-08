Class {
	#name : #ForthCodeZEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZEqu >> execute [ 

	forth push: (forth pop asSigned32 = 0) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeZEqu >> name [

	^ '0='
]
