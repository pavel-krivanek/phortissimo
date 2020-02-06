Class {
	#name : #ForthCodeStoreDSP,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeStoreDSP >> execute [ 

	| address value |
	address := forth pop.
	value := forth pop.
	forth stack at: address put: value 
	
]

{ #category : #accessing }
ForthCodeStoreDSP >> name [

	^ '!dsp'
]
