Class {
	#name : #ForthCodeRBrac,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiling'
}

{ #category : #accessing }
ForthCodeRBrac >> execute [ 

	forth varStateValue: 1
]

{ #category : #accessing }
ForthCodeRBrac >> flags [ 

	^ forth flagImmediate
]

{ #category : #accessing }
ForthCodeRBrac >> name [

	^ ']'
]
