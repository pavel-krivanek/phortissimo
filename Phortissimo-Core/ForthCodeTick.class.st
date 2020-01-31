Class {
	#name : #ForthCodeTick,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiler extending'
}

{ #category : #accessing }
ForthCodeTick >> execute [ 

	forth pcCurrent: (forth wordAt: forth pcNext).
	forth pcNext: forth pcNext + forth wordSize.
	forth push: forth pcCurrent
]

{ #category : #accessing }
ForthCodeTick >> name [

	^ ''''
]
