Class {
	#name : #ForthCodeDSPStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeDSPStore >> execute [ 

	| requestedSize newStack |
	requestedSize := forth pop.
	newStack := OrderedCollection new: requestedSize withAll: 0.
	1 to: (requestedSize min: forth stack size) do: [ :i | newStack at: i put: (forth stack at: i) ].
	forth stack: newStack.
]

{ #category : #accessing }
ForthCodeDSPStore >> name [

	^ 'dsp!'
]
