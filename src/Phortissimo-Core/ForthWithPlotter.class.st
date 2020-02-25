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
	." Phortissimo ready" cr
	
: readLine begin readKeyboard dup 13 <> while dup addToInputBuffer printAscii repeat dup addToInputBuffer printAscii ; 
: quit begin readLine interpret again ; 

quit
	'.

	forth run.
	forth outputBufferString

]

{ #category : #initialization }
ForthWithPlotter >> memoryInitializer [

	^ ForthWithPlotterMemoryInitializer for: self
]

{ #category : #initialization }
ForthWithPlotter >> noInput [

	"ignore"
]

{ #category : #initialization }
ForthWithPlotter >> openPlotter [

	plotter := ForthPlotter new forth: self.
	plotter open.
]

{ #category : #initialization }
ForthWithPlotter >> plotter [

	^ plotter
]
