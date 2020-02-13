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

	self assertCollection: (aCollection flatCollect: #asUnsigned4Bytes) equals: forth stackAsArray
]

{ #category : #running }
ForthTest >> resetStack: aCollection [

	forth resetStack.
	aCollection do: [ :i | 
		forth push: i asUnsigned4Bytes ]
]

{ #category : #running }
ForthTest >> setUp [ 

	super setUp.
	
	forth := Forth new.
	self resetStack: (1 to: 5)
]
