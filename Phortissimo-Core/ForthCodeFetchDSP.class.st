Class {
	#name : #ForthCodeFetchDSP,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeFetchDSP >> execute [ 

	| address |
	address := (forth pop asUnsigned32 - 1) * 4 + 1.
	forth push: (forth stack copyFrom: address to: address + 3).
]

{ #category : #accessing }
ForthCodeFetchDSP >> name [

	^ '@dsp'
]
