Class {
	#name : #ForthCodeStoreDSP,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Data stack'
}

{ #category : #accessing }
ForthCodeStoreDSP >> execute [ 

	| address value |
	address := forth pop asUnsigned32.
	value := forth pop asSigned32.
	forth stack at: address put: value 
	
]

{ #category : #accessing }
ForthCodeStoreDSP >> name [

	^ '!dsp'
]
