Class {
	#name : #ForthCode,
	#superclass : #Object,
	#instVars : [
		'forth'
	],
	#category : #'Phortissimo-Core-Code-Basic'
}

{ #category : #'instance creation' }
ForthCode class >> on: aForth [

	^ self new 
		forth: aForth;
		yourself.
]

{ #category : #accessing }
ForthCode >> execute [ 

	self subclassResponsibility 
]

{ #category : #accessing }
ForthCode >> forth [
	^ forth
]

{ #category : #accessing }
ForthCode >> forth: anObject [
	forth := anObject.

]

{ #category : #accessing }
ForthCode >> installAt: initialPosition [
	
	self installLabelAt: initialPosition.
	forth memoryAt: initialPosition put: self.
	^ initialPosition
]

{ #category : #accessing }
ForthCode >> installLabelAt: position [

	forth addLabel: self label address: position
]

{ #category : #accessing }
ForthCode >> label [ 
	
	^ (self class name withoutPrefix: 'ForthCode') asUppercase
]
