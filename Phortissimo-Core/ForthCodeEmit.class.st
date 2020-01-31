Class {
	#name : #ForthCodeEmit,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-IO'
}

{ #category : #accessing }
ForthCodeEmit >> execute [ 

	forth outputBuffer addLast: forth pop
]

{ #category : #accessing }
ForthCodeEmit >> name [

	^ 'emit'
]
