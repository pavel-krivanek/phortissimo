Class {
	#name : #ForthCodeFromR,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeFromR >> execute [ 

	forth push: (forth popFromReturnStack)
]

{ #category : #accessing }
ForthCodeFromR >> name [

	^ 'r>'
]
