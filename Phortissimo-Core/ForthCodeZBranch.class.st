Class {
	#name : #ForthCodeZBranch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Branching'
}

{ #category : #accessing }
ForthCodeZBranch >> execute [ 

	| value |
	
	value := forth pop asSigned32.
	value = 0 
		ifTrue: [ 
			forth pcNext: forth pcNext + (forth signedWordAt: forth pcNext) ]
		ifFalse: [ 
			forth pcCurrent: (forth wordAt: forth pcNext).
			forth pcNext: forth pcNext + forth wordSize. ]
]

{ #category : #accessing }
ForthCodeZBranch >> name [

	^ '0branch'
]
