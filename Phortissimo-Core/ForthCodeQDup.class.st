Class {
	#name : #ForthCodeQDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeQDup >> execute [ 

	forth stack last = 0
		ifFalse: [ forth push: forth stack last ]
]

{ #category : #accessing }
ForthCodeQDup >> name [

	^ '?dup'
]
