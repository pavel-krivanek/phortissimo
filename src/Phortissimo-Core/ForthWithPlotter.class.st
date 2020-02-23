Class {
	#name : #ForthWithPlotter,
	#superclass : #Forth,
	#instVars : [
		'plotter'
	],
	#category : #'Phortissimo-Core-Basic'
}

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
