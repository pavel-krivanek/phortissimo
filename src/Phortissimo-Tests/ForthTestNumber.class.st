Class {
	#name : #ForthTestNumber,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestNumber >> testBases [

	forth := ForthUppercase new init.
	forth loadStd.
	forth << '
	: TEST
		123                                . CR
		[ HEX -7F ] LITERAL      DECIMAL   . CR
		[ HEX 7FF77FF7 ] LITERAL HEX       . CR
		[ HEX -7FF77FF7 ] LITERAL 2 BASE ! . CR
		[ 2 BASE ! 1111111111101110111111111110111 ] LITERAL HEX . CR
	;
DECIMAL
TEST'.

	forth run.
	self assert: (forth outputBufferString copyReplaceAll: String lf with: String cr) equals: '123 
-127 
7FF77FF7 
-1111111111101110111111111110111 
7FF77FF7 
'


]

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
