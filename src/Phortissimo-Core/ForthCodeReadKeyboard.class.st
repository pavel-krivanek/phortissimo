Class {
	#name : #ForthCodeReadKeyboard,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-IO'
}

{ #category : #accessing }
ForthCodeReadKeyboard >> execute [ 

	| result |
	result := forth privReadKeyboard asUnsigned4Bytes.
	result = 0 ifFalse: [ 
		forth push: result ]
]

{ #category : #accessing }
ForthCodeReadKeyboard >> name [

	^ 'readKeyboard'
]
