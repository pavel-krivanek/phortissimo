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
	forth push: value asUnsigned4Bytes.
		
]

{ #category : #accessing }
ForthCodeLit >> name [

	^ 'lit'
]
