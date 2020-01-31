Class {
	#name : #ForthCodeIncr4,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeIncr4 >> execute [ 

	forth stack at: forth stack size put: (forth stack at: forth stack size) + 4

]

{ #category : #accessing }
ForthCodeIncr4 >> name [

	^ '4+'
]
