Class {
	#name : #ForthTestStack,
	#superclass : #ForthTest,
	#instVars : [
		'memory'
	],
	#category : #'Phortissimo-Tests'
}

{ #category : #running }
ForthTestStack >> setUp [ 
	
	memory := ForthMemory new.
]

{ #category : #running }
ForthTestStack >> testEmpty [

	self assert: memory s0 equals: memory dsp.
	self assert: memory stackSize equals: 0.
]

{ #category : #running }
ForthTestStack >> testPop [

	| content |
	memory push: #(1 2 3 4).
	content := memory pop.
	self assert: content equals: #(1 2 3 4).
	
	self assert: memory dsp equals: memory s0.
	
]

{ #category : #running }
ForthTestStack >> testPush [

	memory push: #(1 2 3 4).
	self assert: memory dsp equals: memory s0 - 4.
	self assert: (memory memoryAt: memory s0-1) equals: 4.
	self assert: (memory memoryAt: memory s0-2) equals: 3.
	self assert: (memory memoryAt: memory s0-3) equals: 2.
	self assert: (memory memoryAt: memory s0-4) equals: 1.

]

{ #category : #running }
ForthTestStack >> testStackAsArray [

	| content |
	memory push: #(1 2 3 4).
	memory push: #(5 6 7 8).
	content := memory stackAsArray.
	self assert: content equals: #(1 2 3 4 5 6 7 8).
	
]
