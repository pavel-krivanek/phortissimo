Class {
	#name : #ForthCodeImmediate,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiler extending'
}

{ #category : #accessing }
ForthCodeImmediate >> execute [ 

	forth toggleFlag: forth flagImmediate of: forth varLatestValue
]

{ #category : #accessing }
ForthCodeImmediate >> flags [ 

	^ forth flagImmediate
]

{ #category : #accessing }
ForthCodeImmediate >> name [

	^ 'immediate'
]
