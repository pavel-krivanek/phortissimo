Class {
	#name : #ForthPlotterCharacterEditor,
	#superclass : #Morph,
	#instVars : [
		'sequences',
		'currentSequence'
	],
	#category : #'Phortissimo-Core-Plotter'
}

{ #category : #'instance creation' }
ForthPlotterCharacterEditor class >> open [ 

	<script>
	Transcript open.
	self new openInWindow.
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> baseline [

	^ 7
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> cellSize [

	^ 40
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> cellsX [

	^ 4
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> cellsY [

	^ 9
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> directionFrom: a to: b [

	| vector |
	vector := b - a.
	vector = ((0)@(-1)) ifTrue: [ ^ #N ]. 	
	vector = ((1)@(-1)) ifTrue: [ ^ #NE ].
	vector = ((1)@(0)) ifTrue: [ ^ #E ].
	vector = ((1)@(1)) ifTrue: [ ^ #SE ].
	vector = ((0)@(1)) ifTrue: [ ^ #S ].
	vector = ((-1)@(1)) ifTrue: [ ^ #SW ].
	vector = ((-1)@(0)) ifTrue: [ ^ #W ].
	vector = ((-1)@(-1)) ifTrue: [ ^ #NW ].


]

{ #category : #drawing }
ForthPlotterCharacterEditor >> drawOn: aCanvas [

	| rect cellSize |
	
	rect := self innerBounds insetBy: 20.
	cellSize := (rect extent x / self cellsX) @ (rect extent y / self cellsY).

	1 to: self cellsY do: [ :y |
		1 to: self cellsX do: [ :x | 
			| aColor |
			aColor := (y = 1 or: [ y > self baseline ])
				ifTrue: [ Color red alpha: 0.1 ]
				ifFalse: [ Color red alpha: 0.4 ].
			
			aCanvas frameRectangle: (rect topLeft + ((cellSize x * (x-1))@(cellSize y *(y-1))) extent: cellSize) color: aColor ] ].
	
	self drawSequencesOn: aCanvas



]

{ #category : #drawing }
ForthPlotterCharacterEditor >> drawSequencesOn: aCanvas [

	| rect cellSize |
	rect := self innerBounds insetBy: 20.
	cellSize := (rect extent x / self cellsX) @ (rect extent y / self cellsY).

	sequences do: [ :sequence | 
		sequence overlappingPairsDo: [ :a :b |
			aCanvas 
				line: ((a-(1@1))*cellSize + self offset + self topLeft)
				to: ((b-(1@1))*cellSize + self offset + self topLeft)
				width: 4
				color: (Color black alpha: 0.2).
			] ]

]

{ #category : #'as yet unclassified' }
ForthPlotterCharacterEditor >> handleKeystroke: evt [

	evt inspect
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> handlesMouseDown: evt [

	^ true
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> initialize [

	super initialize.
	
	self extent: (self cellsX@self cellsY) * self cellSize.
	
	sequences := OrderedCollection new.
	currentSequence := OrderedCollection new.
	sequences add: currentSequence.
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> mouseDown: evt [
	| aPoint coordinates rect cellSize |

	rect := self innerBounds insetBy: 20.
	cellSize := (rect extent x / self cellsX) @ (rect extent y / self cellsY).

	aPoint := evt position - self innerBounds topLeft.
	coordinates := 1 @ 1 + ((aPoint - (self offset @ self offset)) / cellSize) rounded.
	evt whichButton = 4
		ifTrue: [ 
			currentSequence add: coordinates ]
		ifFalse: [
			currentSequence := OrderedCollection new.
			sequences add: currentSequence.
		 ].
	
	self printSequences.
		
	self changed
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> offset [ 

	^ 20
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> printJump: p [

	p = (0@0) ifFalse: [ 
		('{1} {2} penJump' format: {p x. p y}) traceCr ] 
]

{ #category : #drawing }
ForthPlotterCharacterEditor >> printSequences [
	
	| last | 
	
	last := 1@1. 

	Transcript clear.

	sequences do: [ :seq | 
		seq ifNotEmpty: [ 
			| p |
			p := (seq first - last).
			self printJump: p. ].
		(self sequenceString: seq) traceCr.
		seq ifNotEmpty: [last := seq last] ].
	(sequences notEmpty and: [ sequences first notEmpty ])
		ifTrue: [ | p |
			sequences last ifNotEmpty: [ 
				p := sequences last last - (1 @ 1).
				p := (self cellsX - p x)@(0 - p y).
				self printJump: p ]].
	'letterOffset' traceCr.

]

{ #category : #drawing }
ForthPlotterCharacterEditor >> sequenceString: sequence [

	sequence size < 2 ifTrue: [ ^ '' ].
	^ (sequence overlappingPairsCollect: [ :a :b |
		'dr', (self directionFrom: a to: b) ]) joinUsing: String space
]
