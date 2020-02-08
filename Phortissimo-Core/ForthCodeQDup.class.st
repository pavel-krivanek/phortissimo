Class {
	#name : #ForthCodeQDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeQDup >> execute [ 

	forth peek asSigned32 = 0
		ifFalse: [ forth push: forth peek  ]
]

{ #category : #accessing }
ForthCodeQDup >> name [

	^ '?dup'
]
