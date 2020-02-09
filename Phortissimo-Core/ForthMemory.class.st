Class {
	#name : #ForthMemory,
	#superclass : #Object,
	#instVars : [
		'memory',
		'stack',
		'returnStack',
		'forth'
	],
	#category : #'Phortissimo-Core-Basic'
}

{ #category : #memory }
ForthMemory >> addressOfCode: aCodeClass [

	^ memory detect: [ :each | each class = aCodeClass ].
]

{ #category : #memory }
ForthMemory >> byteAt: address [ 

	^ memory at: address
	

]

{ #category : #memory }
ForthMemory >> defineMemory [

	memory := (1 to: self memorySize) collect: [ :each | nil ].
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

	memory := (1 to: self memorySize) collect: [ :each | nil ].
	
	self resetStack.
	
	returnStack := OrderedCollection new.




]

{ #category : #memory }
ForthMemory >> memoryAt: address [

	^ memory at: address
]

{ #category : #memory }
ForthMemory >> memoryAt: address put: anObject [

	^ memory at: address put: anObject
]

{ #category : #memory }
ForthMemory >> memoryCopyFrom: start to: stop [ 

	^ memory copyFrom: start to: stop 
]

{ #category : #memory }
ForthMemory >> memoryReset [

	^ 1 to: memory size do: [ :i | memory at: i put: 0 ].
]

{ #category : #memory }
ForthMemory >> memorySize [

	^ 10*1024
]

{ #category : #memory }
ForthMemory >> peek [

	^ stack last: 4.
]

{ #category : #memory }
ForthMemory >> peekReturnStack [

	^ returnStack last: 4.
]

{ #category : #memory }
ForthMemory >> pop [

	^ (stack removeLast: 4).
]

{ #category : #memory }
ForthMemory >> pop: count [

	^ (stack removeLast: count).
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

	stack addAll: bytes.
	^ bytes
]

{ #category : #memory }
ForthMemory >> pushToReturnStack: address [ 

	returnStack addAll: address asUnsigned4Bytes 
]

{ #category : #memory }
ForthMemory >> resetStack [

	stack := OrderedCollection new.
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
ForthMemory >> stack [ 

	^ stack
]

{ #category : #memory }
ForthMemory >> stack: anObject [
	stack := anObject
]

{ #category : #memory }
ForthMemory >> stackAsArray [

	^ stack asArray
]

{ #category : #memory }
ForthMemory >> stackAt: address [

	^ stack at: address
]

{ #category : #memory }
ForthMemory >> stackAt: address put: anObject [

	^ stack at: address put: anObject
]

{ #category : #memory }
ForthMemory >> stackCopyFrom: start to: end [

	^ stack copyFrom: start to: end
]

{ #category : #memory }
ForthMemory >> stackSize [

	^ stack size
]
