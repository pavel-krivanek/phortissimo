Class {
	#name : #ForthCodeDecr,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDecr >> execute [ 

	forth stack at: forth stack size put: (forth stack at: forth stack size) - 1

]

{ #category : #accessing }
ForthCodeDecr >> name [

	^ '1-'
]
