Class {
	#name : #ForthTestNumber,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestNumber >> testNumber [ 

	| wordCode numberCode |
	wordCode := forth detectCode: ForthCodeWord.
	numberCode := forth detectCode: ForthCodeNumber.
		
	forth << '1'.
	wordCode execute.
	
	numberCode execute.
	
	self assertStack: #(1 2 3 4 5 1 0)
]

{ #category : #tests }
ForthTestNumber >> testSigned [

	self writeReadTest: 0.	
	self writeReadTest: 1.
	self writeReadTest: -1.

	forth writeWord: 16rFFFFFFFF at: 1.
	self assert: (forth signedWordAt: 1) equals: -1.
	self assert:  (forth wordAt: 1) equals: 16rFFFFFFFF
]

{ #category : #tests }
ForthTestNumber >> writeReadTest: n [

	forth writeWord: n at: 1.
	self assert: (forth signedWordAt: 1) equals: n.
]
