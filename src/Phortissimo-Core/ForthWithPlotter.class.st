Class {
	#name : #ForthWithPlotter,
	#superclass : #Forth,
	#instVars : [
		'plotter'
	],
	#category : #'Phortissimo-Core-Basic'
}

{ #category : #example }
ForthWithPlotter class >> example [

	<script>
	| forth |
	forth := ForthWithPlotter new init.
	forth openPlotter.
	forth loadStdWithPlotter.
	forth << '
	." Hello world!" cr
	words
	'.

	forth run.
	forth outputBufferString

]

{ #category : #initialization }
ForthWithPlotter >> memoryInitializer [

	^ ForthWithPlotterMemoryInitializer for: self
]

{ #category : #initialization }
ForthWithPlotter >> openPlotter [

	plotter := ForthPlotter new.
	plotter open.
]

{ #category : #initialization }
ForthWithPlotter >> plotter [

	^ plotter
]
