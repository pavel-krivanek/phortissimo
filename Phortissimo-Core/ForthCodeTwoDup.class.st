Class {
	#name : #ForthCodeTwoDup,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoDup >> execute [ 

	| size a b |
	size := forth stackSize.
	a := forth memoryCopyFrom: forth dsp + 4 to: forth dsp + 7.
	b := forth memoryCopyFrom: forth dsp + 0 to: forth dsp + 3.

	forth push: a.
	forth push: b.

]

{ #category : #accessing }
ForthCodeTwoDup >> name [

	^ '2dup'
]
