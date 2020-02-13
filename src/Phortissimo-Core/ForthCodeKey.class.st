Class {
	#name : #ForthCodeKey,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-IO'
}

{ #category : #accessing }
ForthCodeKey >> execute [ 

	forth push: forth inputBuffer removeFirst asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeKey >> name [

	^ 'key'
]
