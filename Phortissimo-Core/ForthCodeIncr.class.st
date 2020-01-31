Class {
	#name : #ForthCodeIncr,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeIncr >> execute [ 

	forth stack at: forth stack size put: (forth stack at: forth stack size) + 1

]

{ #category : #accessing }
ForthCodeIncr >> name [

	^ '1+'
]
