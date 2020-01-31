Class {
	#name : #ForthTest7Dup,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTest7Dup >> setUp [ 
	
	forth := ForthTest7DupEnvironment new.
	self resetStack: #(1)
]

{ #category : #tests }
ForthTest7Dup >> test7Dup [

	forth pc: 1. "docol" 
	forth pcCurrent: (forth addressForLabel: 'codeword_7DUP').
	forth pcNext: forth pcCurrent + forth wordSize.
	26 timesRepeat: [ forth step ].
	
	self assertStack: #(1 1 1 1 1 1 1 1).

	"forth inputBuffer addAll: ('dup' collect: #asciiValue as: Array)."
	
]

{ #category : #tests }
ForthTest7Dup >> testHide [
	self skip.
	
	forth pc: 1. "docol"
	forth pcCurrent: (forth addressForLabel: 'HIDE').
	forth pcNext: forth pcCurrent + forth wordSize.
	forth inputBuffer addAll: ('dup' collect: #asciiValue as: Array).
	forth step.
	forth step.
	forth step.
	forth step.	
	forth step.
	forth step.	
]
