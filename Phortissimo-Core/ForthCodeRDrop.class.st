Class {
	#name : #ForthCodeRDrop,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeRDrop >> execute [ 

	forth returnStack removeLast
]

{ #category : #accessing }
ForthCodeRDrop >> name [

	^ 'rdrop'
]
