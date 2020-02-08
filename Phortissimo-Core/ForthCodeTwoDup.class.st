Class {
	#name : #ForthCodeTwoDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoDup >> execute [ 

	| size a b |
	size := forth stack size.
	a := forth stack copyFrom: size - 7 to: size - 4.
	b := forth stack copyFrom: size - 3 to: size.

	forth push: a.
	forth push: b.

]

{ #category : #accessing }
ForthCodeTwoDup >> name [

	^ '2dup'
]
