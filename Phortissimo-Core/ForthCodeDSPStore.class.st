Class {
	#name : #ForthCodeDSPStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeDSPStore >> execute [ 

	self shouldBeImplemented 
]

{ #category : #accessing }
ForthCodeDSPStore >> name [

	^ 'dsp!'
]
