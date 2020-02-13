Class {
	#name : #ForthTestFind,
	#superclass : #ForthTest,
	#instVars : [
		'wordCode',
		'findCode'
	],
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestFind >> find: aString expect: labelNameOrZero [
		
	| expected |
	forth inputBuffer addAll: (aString collect: #asciiValue as: Array).
	wordCode execute.
	findCode execute.
	expected := (labelNameOrZero = 0)
		ifTrue: [ 0 ]
		ifFalse: [ forth addressForLabel: labelNameOrZero ].
	
	self assertStack: { 1. 2. 3. 4. 5. expected }
]

{ #category : #tests }
ForthTestFind >> setUp [

	super setUp.
	
	wordCode := forth detectCode: ForthCodeWord.
	findCode := forth detectCode: ForthCodeFind.
]

{ #category : #tests }
ForthTestFind >> testDup [

	self find: 'dup' expect: 'name_DUP'.	

]

{ #category : #tests }
ForthTestFind >> testFind [

	self find: 'find' expect: 'name_FIND'.	

]

{ #category : #tests }
ForthTestFind >> testNotFound [

	self find: 'missingKeyword' expect: 0.	

]
