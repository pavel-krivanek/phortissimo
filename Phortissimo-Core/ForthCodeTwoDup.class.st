Class {
	#name : #ForthCodeTwoDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoDup >> execute [ 

	forth push: (forth stack at: forth stack size - 1).
	forth push: (forth stack at: forth stack size - 1)

]

{ #category : #accessing }
ForthCodeTwoDup >> name [

	^ '2dup'
]
