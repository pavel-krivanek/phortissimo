Class {
	#name : #ForthTestNumber,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestNumber >> testNumber [ 

	| wordCode numberCode |
	wordCode := forth memory detect: [ :each | each class = ForthCodeWord ].
	numberCode := forth memory detect: [ :each | each class = ForthCodeNumber ].
		
	forth << '1'.
	wordCode execute.
	
	numberCode execute.
	
	self assertStack: #(1 2 3 4 5 1 0)
]
