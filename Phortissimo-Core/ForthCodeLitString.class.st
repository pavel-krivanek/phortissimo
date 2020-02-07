Class {
	#name : #ForthCodeLitString,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-String literals'
}

{ #category : #accessing }
ForthCodeLitString >> execute [ 

	forth pcCurrent: (forth wordAt: forth pcNext).
	forth pcNext: forth pcNext + forth wordSize.
	forth push: forth pcNext.
	forth push: forth pcCurrent.
	forth pcNext: (forth pcNext + forth pcCurrent).
	
]

{ #category : #accessing }
ForthCodeLitString >> name [

	^ 'litstring'
]
