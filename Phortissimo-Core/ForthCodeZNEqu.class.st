Class {
	#name : #ForthCodeZNEqu,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Comparison primitives'
}

{ #category : #accessing }
ForthCodeZNEqu >> execute [ 

	forth push: (forth pop ~~ 0) asBit
]

{ #category : #accessing }
ForthCodeZNEqu >> name [

	^ '0<>'
]
