Class {
	#name : #ForthCodeTwoDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoDup >> execute [ 

	| size a b |
	size := forth stackSize.
	a := forth stackCopyFrom: size - 7 to: size - 4.
	b := forth stackCopyFrom: size - 3 to: size.

	forth push: a.
	forth push: b.

]

{ #category : #accessing }
ForthCodeTwoDup >> name [

	^ '2dup'
]
