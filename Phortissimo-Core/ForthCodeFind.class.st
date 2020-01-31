Class {
	#name : #ForthCodeFind,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Dictionary lookup'
}

{ #category : #accessing }
ForthCodeFind >> execute [ 

	| length address toFind |
	
	length := forth pop.
	address := forth pop.
	toFind := forth memory copyFrom: address to: address + length - 1.
	forth push: (self find: toFind)
	
]

{ #category : #accessing }
ForthCodeFind >> find: nameArray [

	| current found |

	current := forth varLatestValue.
	
	[  current = 0 ifTrue: [ ^ 0 ].
		found := forth match: nameArray at: current.
		found ifFalse: [ current := forth wordAt: current ].
		found
	] whileFalse.

	^ current
	
	
	
]

{ #category : #accessing }
ForthCodeFind >> name [

	^ 'find'
]
