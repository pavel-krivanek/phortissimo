Class {
	#name : #ForthCodeFetchDSP,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeFetchDSP >> execute [ 

	| address |
	address := forth pop.
	forth push: (forth stack at: address).
]

{ #category : #accessing }
ForthCodeFetchDSP >> name [

	^ '@dsp'
]
