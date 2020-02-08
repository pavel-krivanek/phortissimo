Class {
	#name : #ForthCodeZNEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZNEqu >> execute [ 

	forth push: (forth pop asSigned32 ~~ 0) asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeZNEqu >> name [

	^ '0<>'
]
