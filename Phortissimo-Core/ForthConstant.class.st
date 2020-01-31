Class {
	#name : #ForthConstant,
	#superclass : #ForthCodeWithHead,
	#instVars : [
		'name',
		'value'
	],
	#category : #'Phortissimo-Core-Code-Variables'
}

{ #category : #'as yet unclassified' }
ForthConstant class >> on: aForth name: aName value: aValue [

	 ^ (self on: aForth)
		name: aName;
		value: aValue;
		yourself
]

{ #category : #accessing }
ForthConstant >> execute [ 

	forth push: value
]

{ #category : #accessing }
ForthConstant >> name [
	^ name
]

{ #category : #accessing }
ForthConstant >> name: anObject [
	name := anObject
]

{ #category : #accessing }
ForthConstant >> value [
	^ value
]

{ #category : #accessing }
ForthConstant >> value: anObject [
	value := anObject
]
