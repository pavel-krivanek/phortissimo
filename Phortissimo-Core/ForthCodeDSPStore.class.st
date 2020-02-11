Class {
	#name : #ForthCodeDSPStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeDSPStore >> execute [ 

	forth memory dsp: forth pop asUnsigned32
]

{ #category : #accessing }
ForthCodeDSPStore >> name [

	^ 'dsp!'
]
