Class {
	#name : #ForthMemoryInitializer,
	#superclass : #Object,
	#instVars : [
		'forth',
		'initPos'
	],
	#category : #'Phortissimo-Core-Basic-Memory initialization'
}

{ #category : #'instance creation' }
ForthMemoryInitializer class >> for: forth [

	^ self new 
		forth: forth;
		yourself
]

{ #category : #accessing }
ForthMemoryInitializer >> addCode: aCode [

	| oldPosition newPosition |
	newPosition := initPos.
	oldPosition := newPosition.
	newPosition := aCode installAt: newPosition.
	initPos := newPosition + 1.
	^ oldPosition
]

{ #category : #accessing }
ForthMemoryInitializer >> forth [
	^ forth
]

{ #category : #accessing }
ForthMemoryInitializer >> forth: anObject [
	forth := anObject
]

{ #category : #accessing }
ForthMemoryInitializer >> initPos [
	^ initPos
]

{ #category : #accessing }
ForthMemoryInitializer >> initPos: anObject [
	initPos := anObject
]

{ #category : #accessing }
ForthMemoryInitializer >> install: codeClass [

	self addCode: (codeClass on: forth).
]

{ #category : #accessing }
ForthMemoryInitializer >> installAll: codeClasses [

	codeClasses do: [ :each | self install: each ]
]
