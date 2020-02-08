Class {
	#name : #ForthCodeDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDup >> execute [ 

	forth push: forth peek

]

{ #category : #accessing }
ForthCodeDup >> name [

	^ 'dup'
]
