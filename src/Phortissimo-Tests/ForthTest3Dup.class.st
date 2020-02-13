Class {
	#name : #ForthTest3Dup,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTest3Dup >> setUp [ 
	
	forth := ForthTest7DupEnvironment new.
	self resetStack: #(1)
]

{ #category : #tests }
ForthTest3Dup >> test3Dup [

	forth pc: 1. "docol" 
	forth pcCurrent: (forth addressForLabel: '3DUP').
	forth pcNext: forth pcCurrent + forth wordSize.
	1+6 timesRepeat: [ forth step ].
	
	self assertStack: #(1 1 1 1).
	
]
