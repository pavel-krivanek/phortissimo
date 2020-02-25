Class {
	#name : #ForthPlotterMorph,
	#superclass : #Morph,
	#instVars : [
		'plotter',
		'form',
		'canvas'
	],
	#category : #'Phortissimo-Core-Plotter'
}

{ #category : #'instance creation' }
ForthPlotterMorph class >> for: aPlotter [

	^ self new initializeFor: aPlotter; yourself
]

{ #category : #accessing }
ForthPlotterMorph >> drawOn: aCanvas [

	aCanvas drawImage: form at: self topLeft
]

{ #category : #accessing }
ForthPlotterMorph >> form [
	^ form
]

{ #category : #accessing }
ForthPlotterMorph >> form: anObject [
	form := anObject
]

{ #category : #keyboard }
ForthPlotterMorph >> handlesKeyboard: evt [
	^true
]

{ #category : #accessing }
ForthPlotterMorph >> initialize [

	super initialize.
	
]

{ #category : #accessing }
ForthPlotterMorph >> initializeFor: aPlotter [

	plotter := aPlotter.
	self extent: aPlotter extent.
	form := Form extent: plotter extent depth: 2.
	
	form fillWhite.
	canvas := FormCanvas on: form.
	
]

{ #category : #keyboard }
ForthPlotterMorph >> keyStroke: event [ 

	self plotter forth addToKeyboardBuffer: event keyCharacter
]

{ #category : #drawing }
ForthPlotterMorph >> line: fromPoint to: toPoint color: aColor [

	canvas line: fromPoint to: toPoint color: aColor.
	self changed.
]

{ #category : #accessing }
ForthPlotterMorph >> plotter [
	^ plotter
]

{ #category : #accessing }
ForthPlotterMorph >> plotter: anObject [
	plotter := anObject
]

{ #category : #keyboard }
ForthPlotterMorph >> takesKeyboardFocus [
	"Answer whether the receiver can normally take keyboard focus."

	^true
]
