Class {
	#name : #ForthCodeDrop,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Basic primitives'
}

{ #category : #accessing }
ForthCodeDrop >> execute [ 

	forth pop
]

{ #category : #accessing }
ForthCodeDrop >> name [

	^ 'drop'
]
