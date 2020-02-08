Class {
	#name : #ForthCodeDSPFetch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeDSPFetch >> execute [ 

	forth push: ((forth stack size + 4) // 4) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeDSPFetch >> name [

	^ 'dsp@'
]
