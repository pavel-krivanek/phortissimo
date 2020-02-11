Class {
	#name : #ForthCodeOver,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeOver >> execute [ 

	forth push: (forth memoryCopyFrom: forth dsp + 4 to: forth dsp + 7)
]

{ #category : #accessing }
ForthCodeOver >> name [

	^ 'over'
]
