Class {
	#name : #ForthCodeDSPFetch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeDSPFetch >> execute [ 

	forth push: (16rFFFFFFFF - (forth stackSize)) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeDSPFetch >> name [

	^ 'dsp@'
]
