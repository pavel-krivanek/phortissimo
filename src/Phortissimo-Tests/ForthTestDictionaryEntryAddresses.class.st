Class {
	#name : #ForthTestDictionaryEntryAddresses,
	#superclass : #ForthTest,
	#instVars : [
		'wordCode',
		'findCode'
	],
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestDictionaryEntryAddresses >> findCFA: wordLabel expect: codewordLabel [
		
	| expected cfaCode |
	cfaCode := forth detectCode: ForthCodeTCFA.
	forth push: (forth addressForLabel: wordLabel) asUnsigned4Bytes.	
	expected := forth addressForLabel: codewordLabel.
	cfaCode execute.
	
	self assertStack: { 1. 2. 3. 4. 5. expected }
]

{ #category : #tests }
ForthTestDictionaryEntryAddresses >> findDFA: wordLabel expect: codewordLabel [
		
	| expected cfaCode |
	cfaCode := forth detectCode: ForthCodeTDFA.
	forth push: (forth addressForLabel: wordLabel) asUnsigned4Bytes.	
	expected := forth addressForLabel: codewordLabel.
	cfaCode execute.
	
	self assertStack: { 1. 2. 3. 4. 5. expected }
]

{ #category : #tests }
ForthTestDictionaryEntryAddresses >> testDupCFA [

	self findCFA: 'name_DUP' expect: 'codeword_DUP'.	

]

{ #category : #tests }
ForthTestDictionaryEntryAddresses >> testDupDFA [

	self findDFA: 'name_DUP' expect: 'code_DUP'.	

]

{ #category : #tests }
ForthTestDictionaryEntryAddresses >> testFindCFA [

	self findCFA: 'name_FIND' expect: 'codeword_FIND'.	

]

{ #category : #tests }
ForthTestDictionaryEntryAddresses >> testFindDFA [

	self findDFA: 'name_FIND' expect: 'code_FIND'.	

]
