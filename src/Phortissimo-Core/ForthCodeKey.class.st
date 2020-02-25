Class {
	#name : #ForthCodeKey,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-IO'
}

{ #category : #accessing }
ForthCodeKey >> execute [ 

	| input |
	input := forth readInputBuffer.
	self forth isRunning ifTrue: [ forth push: input asUnsigned4Bytes ]
]

{ #category : #accessing }
ForthCodeKey >> name [

	^ 'key'
]
