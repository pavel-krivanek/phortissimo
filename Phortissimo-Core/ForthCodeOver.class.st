Class {
	#name : #ForthCodeOver,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeOver >> execute [ 

	forth push: (forth stack at: forth stack size - 1)

]

{ #category : #accessing }
ForthCodeOver >> name [

	^ 'over'
]
