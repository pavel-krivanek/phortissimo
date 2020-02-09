Class {
	#name : #ForthCodeDSPStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeDSPStore >> execute [ 

	| requestedSize newStack address |
	address := forth pop asUnsigned32.
	requestedSize := 16rFFFFFFFF - address.
	newStack := OrderedCollection new: requestedSize withAll: 0.
	1 to: (requestedSize min: forth stackSize) do: [ :i | newStack at: i put: (forth stackAt: i) ].
	forth stack: newStack.
]

{ #category : #accessing }
ForthCodeDSPStore >> name [

	^ 'dsp!'
]
