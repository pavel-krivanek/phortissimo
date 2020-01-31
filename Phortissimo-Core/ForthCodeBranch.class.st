Class {
	#name : #ForthCodeBranch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Branching'
}

{ #category : #accessing }
ForthCodeBranch >> execute [ 

	forth pcNext: forth pcNext + (forth signedWordAt: forth pcNext)
]

{ #category : #accessing }
ForthCodeBranch >> name [

	^ 'branch'
]
