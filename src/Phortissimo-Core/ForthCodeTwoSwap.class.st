Class {
	#name : #ForthCodeTwoSwap,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoSwap >> execute [ 

	| a b c d |
	
	a := forth pop asSigned32.	
	b := forth pop asSigned32.
	c := forth pop asSigned32.
	d := forth pop asSigned32.
	forth push: b asUnsigned4Bytes.
	forth push: a asUnsigned4Bytes.	
	forth push: d asUnsigned4Bytes.
	forth push: c asUnsigned4Bytes.
]

{ #category : #accessing }
ForthCodeTwoSwap >> name [

	^ '2swap'
]
