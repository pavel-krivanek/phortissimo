Class {
	#name : #ForthCodeLBrac,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiling'
}

{ #category : #accessing }
ForthCodeLBrac >> execute [ 

	forth varStateValue: 0
]

{ #category : #accessing }
ForthCodeLBrac >> flags [ 

	^ forth flagImmediate
]

{ #category : #accessing }
ForthCodeLBrac >> name [

	^ '['
]
