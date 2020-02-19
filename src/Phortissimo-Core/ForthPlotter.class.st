Class {
	#name : #ForthPlotter,
	#superclass : #Object,
	#instVars : [
		'form',
		'position',
		'direction',
		'busy',
		'morph',
		'pen',
		'window',
		'surface',
		'canvas'
	],
	#category : #'Phortissimo-Core-Plotter'
}

{ #category : #example }
ForthPlotter class >> example [

	<script>
	| p |
	p := ForthPlotter new.
	p open. p down.p right.
	100 timesRepeat: [ p step].
	p penDown. p down.p right .
	100 timesRepeat: [  p step].
	p penUp. p down. p right .
	100 timesRepeat: [ p step].
	p penDown. p down. p right .
	100 timesRepeat: [  p step].

	p update
]

{ #category : #accessing }
ForthPlotter >> busy [
	^ busy
]

{ #category : #accessing }
ForthPlotter >> busy: anObject [
	busy := anObject
]

{ #category : #accessing }
ForthPlotter >> direction [
	^ direction
]

{ #category : #accessing }
ForthPlotter >> direction: anObject [
	direction := anObject
]

{ #category : #'instance creation' }
ForthPlotter >> down [

	direction := (direction y)@1
]

{ #category : #'instance creation' }
ForthPlotter >> extent [ 
	
	^ 400@200
]

{ #category : #accessing }
ForthPlotter >> form [
	^ form
]

{ #category : #'instance creation' }
ForthPlotter >> home [ 

	self penUp.
	self position: 0@0.
]

{ #category : #initialization }
ForthPlotter >> initialize [

	super initialize.
	form := Form extent: self extent depth: 2.
	form fillWhite.
	canvas := FormCanvas on: form.
	position := 0@0.
	pen := false.
	direction := 0@0.
	busy := false.
]

{ #category : #'instance creation' }
ForthPlotter >> left [

	direction := -1@(direction y)
]

{ #category : #'instance creation' }
ForthPlotter >> limitted: aPoint [

	^ (aPoint x min: (self extent x-1) max: 0) @ (aPoint y min: (self extent y-1) max: 0)
]

{ #category : #'instance creation' }
ForthPlotter >> open [ 
	
	morph := form asMorph.
	window := morph openInWindow.
]

{ #category : #'instance creation' }
ForthPlotter >> penDown [

	pen := true.
]

{ #category : #'instance creation' }
ForthPlotter >> penUp [

	pen := false.
]

{ #category : #accessing }
ForthPlotter >> position [
	^ position
]

{ #category : #accessing }
ForthPlotter >> position: anObject [
	position := anObject
]

{ #category : #'instance creation' }
ForthPlotter >> resetDirection [

	direction := 0@0
]

{ #category : #'instance creation' }
ForthPlotter >> right [

	direction := 1@(direction y)
]

{ #category : #'instance creation' }
ForthPlotter >> step [ 

	| limitted |

	limitted := self limitted: position + direction.
	
	pen ifFalse: [ 
		position := limitted.
		^ self ].
	
	canvas line: position to: limitted color: Color black.
	position := limitted.
	
	morph changed.

	

	
]

{ #category : #'instance creation' }
ForthPlotter >> top [

	direction := (direction y)@(-1)
]

{ #category : #'instance creation' }
ForthPlotter >> update [

	morph changed
]
