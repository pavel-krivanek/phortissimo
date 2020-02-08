Class {
	#name : #Forth,
	#superclass : #Object,
	#instVars : [
		'memory',
		'stack',
		'returnStack',
		'pc',
		'pcNext',
		'pcCurrent',
		'initPos',
		'labels',
		'lastWord',
		'inputBuffer',
		'outputBuffer',
		'unknownLabels',
		'state'
	],
	#category : #'Phortissimo-Core-Basic'
}

{ #category : #streaming }
Forth >> << aString [

	self inputBuffer addAll: (aString collect: #asciiValue as: Array).
]

{ #category : #accessing }
Forth >> addLabel: aLabel address: anAddress [

	^ labels at: aLabel put: anAddress

]

{ #category : #accessing }
Forth >> addressForLabel: aLabel [

	^ labels at: aLabel
]

{ #category : #accessing }
Forth >> addressForLabel: aLabel inFutureSet: address [

	^ labels at: aLabel ifAbsent: [ 
		self registerUnknownLabel: aLabel usageAtPosition: address.
		0 ]
]

{ #category : #initialization }
Forth >> byteAt: address [ 

	^ memory at: address
	

]

{ #category : #'as yet unclassified' }
Forth >> codewordOf: dictionaryWordAddress [

	| current length | 
	current := dictionaryWordAddress + self wordSize.
	length := self lengthByteAt: current.
	
	^ current + 1 + length
]

{ #category : #accessing }
Forth >> find: nameArray [

	| current found |

	current := self varLatestValue.
	
	[  current = 0 ifTrue: [ ^ 0 ].
		found := (self match: nameArray at: current) and: [(self isHidden: current) not].
		found ifFalse: [ current := self wordAt: current ].
		found
	] whileFalse.

	^ current
	
	
	
]

{ #category : #initialization }
Forth >> fixUnknownLabels [

	unknownLabels keysAndValuesDo: [ :label :usages |
		| correctAddress |
		correctAddress := self addressForLabel: label.
		usages do: [ :usageAddress | self writeWord: correctAddress at: usageAddress ] ].
	
	unknownLabels := Dictionary new.
]

{ #category : #accessing }
Forth >> flagHidden [

	^ 16r20
]

{ #category : #accessing }
Forth >> flagImmediate [

	^ 16r80
]

{ #category : #accessing }
Forth >> flagLengthMask [

	^ 16r1F
]

{ #category : #initialization }
Forth >> init [ 

	self pc: 1. "docol" 
	self pcCurrent: (self addressForLabel: 'codeword_QUIT').
	self pcNext: self pcCurrent + self wordSize.
	self varStateValue: 0.

]

{ #category : #initialization }
Forth >> initPos [

	^ initPos
]

{ #category : #accessing }
Forth >> initPos: anObject [
	initPos := anObject
]

{ #category : #initialization }
Forth >> initialize [

	memory := (1 to: 10*1024) collect: [ :each | nil ].
	self resetStack.
	returnStack := OrderedCollection new.
	labels := Dictionary new.
	unknownLabels := Dictionary new.
	lastWord := 0.
	
	inputBuffer := OrderedCollection new.
	outputBuffer := OrderedCollection new.
		
	self memoryInitializer initializeMemory.
	
	state := #running.

]

{ #category : #initialization }
Forth >> inputBuffer [

	^ inputBuffer 
]

{ #category : #'as yet unclassified' }
Forth >> isHidden: dictionaryWordAddress [

	^ ((self byteAt: (dictionaryWordAddress + self wordSize)) bitAnd: self flagHidden) ~= 0. 
]

{ #category : #'as yet unclassified' }
Forth >> isImmediate: dictionaryWordAddress [

	^ ((self byteAt: (dictionaryWordAddress + self wordSize)) bitAnd: self flagImmediate) ~= 0. 
]

{ #category : #accessing }
Forth >> labels [ 

	^ labels
]

{ #category : #accessing }
Forth >> labels: anObject [
	labels := anObject
]

{ #category : #'as yet unclassified' }
Forth >> labelsFor: index [ 

	^ labels associations select: [ :assoc |
		assoc value = index ] 
]

{ #category : #accessing }
Forth >> lastWord [
	^ lastWord
]

{ #category : #accessing }
Forth >> lastWord: anObject [
	lastWord := anObject
]

{ #category : #initialization }
Forth >> lengthByteAt: address [ 

	^ (memory at: address) bitAnd: self lengthMask
	

]

{ #category : #accessing }
Forth >> lengthMask [

	^ 16r1F
]

{ #category : #accessing }
Forth >> match: nameArray at: address [

	| length anArray |
	length := self lengthByteAt: address + self wordSize.
	length = nameArray size ifFalse: [ ^ false ].
	anArray := self memory copyFrom: address + self wordSize + 1 to: address + self wordSize + nameArray size.
	^ nameArray = anArray
	
	
	
	
	
]

{ #category : #accessing }
Forth >> memory [

	^ memory
]

{ #category : #accessing }
Forth >> memory: anObject [
	memory := anObject
]

{ #category : #initialization }
Forth >> memoryInitializer [

	^ ForthStandardMemoryInitializer for: self
]

{ #category : #initialization }
Forth >> noInput [

	state := #noInput
]

{ #category : #initialization }
Forth >> outputBuffer [

	^ outputBuffer 
]

{ #category : #accessing }
Forth >> pc [
	^ pc
]

{ #category : #accessing }
Forth >> pc: anObject [
	pc := anObject
]

{ #category : #accessing }
Forth >> pcCurrent [
	^ pcCurrent
]

{ #category : #accessing }
Forth >> pcCurrent: anObject [
	pcCurrent := anObject
]

{ #category : #accessing }
Forth >> pcNext [
	^ pcNext
]

{ #category : #accessing }
Forth >> pcNext: anObject [
	pcNext := anObject
]

{ #category : #initialization }
Forth >> peek [

	^ stack last: 4.
]

{ #category : #initialization }
Forth >> peekReturnStack [

	^ returnStack last: 4.
]

{ #category : #initialization }
Forth >> pop [

	^ (stack removeLast: 4).
]

{ #category : #initialization }
Forth >> popFromReturnStack: address [ 

	^ returnStack removeLast: 4
]

{ #category : #'as yet unclassified' }
Forth >> printMemoryOn: aStream [

	memory withIndexDo: [ :each :index |
		index printOn: aStream  base: 10 length: 6 padded: false.
		aStream space.
		(self labelsFor: index) ifNotEmpty: [ :cellLabels |
			aStream << (cellLabels joinUsing: String space) << ':' ; cr; << '       ' ].
		aStream tab; tab.
		each printOn: aStream. aStream cr] 
		 
]

{ #category : #'as yet unclassified' }
Forth >> privComma: value [

	self writeWord: value at: self varHereValue.
	self varHereValue: self varHereValue + self wordSize.
]

{ #category : #initialization }
Forth >> privNext [
	
	self pcCurrent: (self wordAt: self pcNext).
	self pcNext: self pcNext + self wordSize.
	self pc: (self wordAt: self pcCurrent) - 1.
	"subtract one because the address will be immediately increased"
]

{ #category : #'as yet unclassified' }
Forth >> privNumber: wordStringAddress length: length [

	| aString base number |

	base := self signedWordAt: (self addressForLabel: 'var_BASE').

	aString := String withAll: ((self  memory copyFrom: wordStringAddress to: wordStringAddress + (length-1)) 
		withIndexCollect: [ :each :index | 		
			((each >= 48 and: [ each < (48+base) ]) or: [ each = 45 "-" ])
				ifFalse: [ ^ number -> index ].
			Character value: each ]).
	
	number := Number readFrom: aString readStream base: base.
	
	^ number -> 0 "no error"
	
]

{ #category : #accessing }
Forth >> privWord [

	| length character characterCode|
	
	length := 0.
	
	
	[  self inputBuffer isEmpty ifTrue: [ 
			self noInput.
			^ self wordBufferAddress -> 0 ].

		characterCode := self inputBuffer removeFirst.
		character := Character value: characterCode.
		character isSeparator.
	] whileTrue.

	[  | result |
		self memory at: (self wordBufferAddress + length) put: characterCode.
		length := length + 1.
		result := self inputBuffer isEmpty 
			ifFalse: [ 
				characterCode := self inputBuffer removeFirst.
				character := Character value: characterCode.
				character isSeparator ] 
			ifTrue: [ true ].
	] whileFalse.

	(length = 1 and: [ (self byteAt: self wordBufferAddress) = ($\ asciiValue) ]) ifTrue: [ 
		[ 
		self inputBuffer isEmpty ifTrue: [ 
			self noInput.
			^ self wordBufferAddress -> 0 ].
		characterCode := self inputBuffer removeFirst.
		character := Character value: characterCode.
		(character = Character cr) or: [ character = Character lf ] ] whileFalse.
		^ self privWord ].

	^ self wordBufferAddress -> length
	
	
	
]

{ #category : #initialization }
Forth >> push: bytes [

	stack addAll: bytes.
	^ bytes
]

{ #category : #initialization }
Forth >> pushToReturnStack: address [ 

	returnStack addAll: address asUnsigned4Bytes 
]

{ #category : #accessing }
Forth >> registerUnknownLabel: aLabel usageAtPosition: address [

	(unknownLabels at: aLabel ifAbsentPut: [ Set new ]) add: address
]

{ #category : #initialization }
Forth >> resetStack [

	stack := OrderedCollection new.
]

{ #category : #initialization }
Forth >> returnStack [ 

	^ returnStack
]

{ #category : #accessing }
Forth >> returnStack: anObject [
	returnStack := anObject
]

{ #category : #initialization }
Forth >> run [ 

	[ state = #running ] whileTrue: [ self step ]
]

{ #category : #initialization }
Forth >> signedWordAt: address [ 

	| unsigned |
	
	unsigned := ((memory at: address + 3)) +
	  ((memory at: address + 2) <<  8) +
	  ((memory at: address + 1) << 16) +
	  ((memory at: address ) << 24).
		
	^ unsigned asSigned32
	


]

{ #category : #initialization }
Forth >> stack [ 

	^ stack
]

{ #category : #accessing }
Forth >> stack: anObject [
	stack := anObject
]

{ #category : #initialization }
Forth >> step [ 

"	('pc: ', pc asString, ' ', (self labels associations select: [ :e | e value = pc ]) asString)  traceCr."
	(memory at: pc) execute.
	pc := pc + 1.
]

{ #category : #initialization }
Forth >> toggleFlag: flag of: wordAddress [
	
	| flagAddress |
	flagAddress := wordAddress + self wordSize.
	self writeByte: ((self byteAt: flagAddress) bitXor: flag) at: flagAddress
]

{ #category : #accessing }
Forth >> unknownLabels [

	^ unknownLabels
]

{ #category : #accessing }
Forth >> uppercase [
	^ false
]

{ #category : #initialization }
Forth >> varHere [

	^ self addressForLabel: 'var_HERE'
]

{ #category : #initialization }
Forth >> varHereValue [

	^ self wordAt: self varHere
]

{ #category : #initialization }
Forth >> varHereValue: aValue [

	^ self writeWord: aValue at: self varHere
]

{ #category : #initialization }
Forth >> varLatest [

	^ self addressForLabel: 'var_LATEST'
]

{ #category : #initialization }
Forth >> varLatestValue [

	^ self wordAt: self varLatest
]

{ #category : #initialization }
Forth >> varLatestValue: aValue [

	^ self writeWord: aValue at: self varLatest
]

{ #category : #initialization }
Forth >> varState [

	^ self addressForLabel: 'var_STATE'
]

{ #category : #initialization }
Forth >> varStateValue [

	^ self signedWordAt: self varState
]

{ #category : #initialization }
Forth >> varStateValue: aValue [

	^ self writeWord: aValue at: self varState
]

{ #category : #initialization }
Forth >> wordAt: address [ 

	^ ((memory at: address + 3)) +
	  ((memory at: address + 2) <<  8) +
	  ((memory at: address + 1) << 16) +
	  ((memory at: address + 0) << 24) 
	


]

{ #category : #accessing }
Forth >> wordBufferAddress [

	^ self addressForLabel: 'word_buffer'
]

{ #category : #accessing }
Forth >> wordBufferSize [

	^ 32
]

{ #category : #initialization }
Forth >> wordSize [

	^ 4
]

{ #category : #initialization }
Forth >> writeByte: aByte at: address [ 

	memory at: address put: aByte
	

]

{ #category : #initialization }
Forth >> writeCode: aCode at: address [ 

	memory at: address put: aCode
	

]

{ #category : #initialization }
Forth >> writeWord: aWord at: address [ 

	| num bytes |
	
	num := aWord asUnsigned32.
	
	bytes := num asByteArrayOfSize: 4.

	memory at: (address+0) put: (bytes at: 1).
	memory at: (address+1) put: (bytes at: 2).
	memory at: (address+2) put: (bytes at: 3).
	memory at: (address+3) put: (bytes at: 4).
	


]
