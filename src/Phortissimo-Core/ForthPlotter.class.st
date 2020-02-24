Class {
	#name : #ForthPlotter,
	#superclass : #Object,
	#instVars : [
		'forth',
		'position',
		'direction',
		'busy',
		'morph',
		'pen',
		'window'
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

	direction := (direction x)@1
]

{ #category : #'instance creation' }
ForthPlotter >> extent [ 
	
	^ 800@800
]

{ #category : #accessing }
ForthPlotter >> forth [
	^ forth
]

{ #category : #accessing }
ForthPlotter >> forth: anObject [
	forth := anObject
]

{ #category : #'instance creation' }
ForthPlotter >> home [ 

	self penUp.
	self position: 0@0.
]

{ #category : #initialization }
ForthPlotter >> initialize [

	super initialize.
	position := 0@0.
	pen := false.
	direction := 0@0.
	busy := false.
	morph := ForthPlotterMorph for: self.
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
	
	morph line: position to: limitted color: Color black.
	position := limitted.
	

	

	
]

{ #category : #'instance creation' }
ForthPlotter >> top [

	direction := (direction y)@(-1)
]

{ #category : #'instance creation' }
ForthPlotter >> up [

	direction := (direction x)@(-1)
]

{ #category : #'instance creation' }
ForthPlotter >> update [

	morph changed
]
