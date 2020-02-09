Class {
	#name : #ForthCodeFetchDSP,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeFetchDSP >> execute [ 

	| address |
	address := 16rFFFFFFFF-(forth pop asUnsigned32)-3.
	forth push: (forth stack copyFrom: address to: address + 3).
]

{ #category : #accessing }
ForthCodeFetchDSP >> name [

	^ '@dsp'
]
