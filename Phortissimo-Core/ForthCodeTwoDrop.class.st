Class {
	#name : #ForthCodeTwoDrop,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeTwoDrop >> execute [ 

	forth stack removeLast: 2
]

{ #category : #accessing }
ForthCodeTwoDrop >> name [

	^ '2drop'
]