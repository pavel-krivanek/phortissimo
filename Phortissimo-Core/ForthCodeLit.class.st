Class {
	#name : #ForthCodeLit,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Literals'
}

{ #category : #accessing }
ForthCodeLit >> execute [ 

	| value |
	value := forth wordAt: forth pcNext.
	forth pcCurrent: forth pcNext.
	forth pcNext: forth pcNext + forth wordSize.
	forth push: value.
		
]

{ #category : #accessing }
ForthCodeLit >> name [

	^ 'lit'
]
