Class {
	#name : #ForthCodeAddToInputBuffer,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-IO'
}

{ #category : #accessing }
ForthCodeAddToInputBuffer >> execute [ 

	forth inputBuffer addLast: forth pop asUnsigned32
]

{ #category : #accessing }
ForthCodeAddToInputBuffer >> name [

	^ 'addToInputBuffer'
]
