Class {
	#name : #ForthMemory,
	#superclass : #Object,
	#instVars : [
		'segments',
		'dsp',
		'returnStack',
		'forth'
	],
	#category : #'Phortissimo-Core-Basic'
}

{ #category : #memory }
ForthMemory >> byteAt: address [ 

	^ self memoryAt: address
]

{ #category : #memory }
ForthMemory >> detectCode: aCodeClass [

	1 to: self memorySize do: [ :index |
		| value |
		((value := self memoryAt: index) class = aCodeClass) ifTrue: [ ^ value ] ].
]

{ #category : #memory }
ForthMemory >> dsp [

	^ dsp
]

{ #category : #memory }
ForthMemory >> dsp: anAddress [

	dsp := anAddress
]

{ #category : #memory }
ForthMemory >> ensureSegmentOf: address [ 

	^ segments at: (self segmentIdOf: address) ifAbsentPut: [ self newSegment ].
]

{ #category : #accessing }
ForthMemory >> forth [
	^ forth
]

{ #category : #accessing }
ForthMemory >> forth: anObject [
	forth := anObject
]

{ #category : #memory }
ForthMemory >> initialize [ 

	self memoryReset.
	
	self resetStack.
	
	returnStack := OrderedCollection new.




]

{ #category : #memory }
ForthMemory >> memoryAt: address [ 

	| segment |
	segment := self segmentOf: address.
	segment ifNil: [ ^ 0 ].
	^ segment at: (self offsetOf: address)+1

]

{ #category : #memory }
ForthMemory >> memoryAt: address put: anObject [

	^ (self ensureSegmentOf: address) at: (self offsetOf: address)+1 put: anObject
]

{ #category : #memory }
ForthMemory >> memoryCopyFrom: start to: stop [ 

	| result |
	result := Array ofSize: (stop-start+1).
	start to: stop do: [ :i | 
		result at: i-start+1 put: (self memoryAt: i) ].
	^ result
]

{ #category : #memory }
ForthMemory >> memoryReset [

	^ segments := IdentityDictionary new
]

{ #category : #memory }
ForthMemory >> memorySize [

	^ 10*1024
]

{ #category : #memory }
ForthMemory >> newSegment [

	^ Array ofSize: 16rFFF+1
]

{ #category : #memory }
ForthMemory >> offsetOf: address [ 

	^ address bitAnd: 16rFFF
]

{ #category : #memory }
ForthMemory >> peek [

	^ self memoryCopyFrom: dsp to: dsp+3.
]

{ #category : #memory }
ForthMemory >> peekReturnStack [

	^ returnStack last: 4.
]

{ #category : #memory }
ForthMemory >> pop [

	^ self pop: 4
]

{ #category : #memory }
ForthMemory >> pop: count [

	| result | 
	result := self memoryCopyFrom: dsp to: dsp+count-1.
	dsp := dsp + count.
	^ result
]

{ #category : #memory }
ForthMemory >> popFromReturnStack [

	^ returnStack removeLast: 4
]

{ #category : #memory }
ForthMemory >> printMemoryOn: aStream [

	memory withIndexDo: [ :each :index |
		index printOn: aStream  base: 10 length: 6 padded: false.
		aStream space.
		(self forth labelsFor: index) ifNotEmpty: [ :cellLabels |
			aStream << (cellLabels joinUsing: String space) << ':' ; cr; << '       ' ].
		aStream tab; tab.
		each printOn: aStream. aStream cr] 
		 
]

{ #category : #memory }
ForthMemory >> push: bytes [

	dsp := dsp - 4.
	bytes withIndexDo: [ :each :index |
		self memoryAt: dsp+index-1 put: each ].
	^ bytes
]

{ #category : #memory }
ForthMemory >> pushToReturnStack: address [ 

	returnStack addAll: address asUnsigned4Bytes 
]

{ #category : #memory }
ForthMemory >> resetStack [

	dsp := self s0.
]

{ #category : #memory }
ForthMemory >> returnStackPush: anArray [

	^ returnStack addAll: anArray
]

{ #category : #memory }
ForthMemory >> returnStackSize [

	^ returnStack size
]

{ #category : #memory }
ForthMemory >> s0 [

	^ 16rFFFFFFFC
]

{ #category : #memory }
ForthMemory >> segmentIdOf: address [ 

	^ address bitAnd: 16rFFFFF000.	

]

{ #category : #memory }
ForthMemory >> segmentOf: address [ 

	^ segments at: (self segmentIdOf: address) ifAbsent: nil
]

{ #category : #memory }
ForthMemory >> stackAsArray [

	^ ((self memoryCopyFrom: dsp to: self s0-1) groupsOf: 4 atATimeCollect: [ :x | x ]) reversed flatCollect: #yourself
]

{ #category : #memory }
ForthMemory >> stackAsUnsignedArray [

	^ ((self memoryCopyFrom: dsp to: self s0-1) groupsOf: 4 atATimeCollect: [ :x | x asUnsigned32 ]) reversed 
]

{ #category : #memory }
ForthMemory >> stackSize [

	^ dsp - self s0
]
