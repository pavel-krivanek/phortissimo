Class {
	#name : #ForthVariable,
	#superclass : #ForthCodeWithHead,
	#instVars : [
		'name',
		'initialValue',
		'address'
	],
	#category : #'Phortissimo-Core-Code-Variables'
}

{ #category : #'as yet unclassified' }
ForthVariable class >> on: aForth name: aName [

	^ self on: aForth name: aName initialValue: 0
]

{ #category : #'as yet unclassified' }
ForthVariable class >> on: aForth name: aName initialValue: aValue [

	 ^ (self on: aForth)
		name: aName;
		initialValue: aValue;
		yourself
]

{ #category : #accessing }
ForthVariable >> execute [ 

	forth push: address
]

{ #category : #accessing }
ForthVariable >> finishAt: initialPosition [

	| newPosition |
		
	newPosition := super finishAt: initialPosition.

	newPosition := newPosition + 1.
	forth addLabel: 'var_', self name asUppercase address: newPosition.
	forth writeWord: self initialValue at: newPosition.
	address := newPosition.
	newPosition := newPosition + forth wordSize.
	^ newPosition.

]

{ #category : #accessing }
ForthVariable >> initialValue [

	^ initialValue ifNil: [ 0 ]
]

{ #category : #accessing }
ForthVariable >> initialValue: anObject [
	initialValue := anObject
]

{ #category : #accessing }
ForthVariable >> label [ 

	^ self name asUppercase
]

{ #category : #accessing }
ForthVariable >> name [
	^ name
]

{ #category : #accessing }
ForthVariable >> name: anObject [
	name := anObject
]
