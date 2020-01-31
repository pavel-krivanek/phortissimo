Class {
	#name : #ForthTest,
	#superclass : #TestCase,
	#instVars : [
		'forth'
	],
	#category : #'Phortissimo-Tests'
}

{ #category : #running }
ForthTest >> assertStack: aCollection [

	self assertCollection: aCollection equals: forth stack asArray
]

{ #category : #running }
ForthTest >> resetStack: aCollection [

	forth resetStack.
	aCollection do: [ :i | 
		forth push: i ]
]

{ #category : #running }
ForthTest >> setUp [ 

	super setUp.
	
	forth := Forth new.
	self resetStack: (1 to: 5)
]
