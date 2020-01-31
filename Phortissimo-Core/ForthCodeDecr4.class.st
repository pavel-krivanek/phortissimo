Class {
	#name : #ForthCodeDecr4,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDecr4 >> execute [ 

	forth stack at: forth stack size put: (forth stack at: forth stack size) - 4

]

{ #category : #accessing }
ForthCodeDecr4 >> name [

	^ '4-'
]
