Class {
	#name : #ForthCodeRDrop,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeRDrop >> execute [ 

	forth popFromReturnStack
]

{ #category : #accessing }
ForthCodeRDrop >> name [

	^ 'rdrop'
]
