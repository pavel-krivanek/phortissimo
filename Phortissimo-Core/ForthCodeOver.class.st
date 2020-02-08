Class {
	#name : #ForthCodeOver,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeOver >> execute [ 

	| size | 
	size := forth stack size.
	forth push: (forth stack copyFrom: size - 7 to: size - 4)

]

{ #category : #accessing }
ForthCodeOver >> name [

	^ 'over'
]
