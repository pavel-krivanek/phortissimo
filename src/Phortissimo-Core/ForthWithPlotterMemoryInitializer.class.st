Class {
	#name : #ForthWithPlotterMemoryInitializer,
	#superclass : #ForthStandardMemoryInitializer,
	#category : #'Phortissimo-Core-Basic-Memory initialization'
}

{ #category : #initialization }
ForthWithPlotterMemoryInitializer >> initializeBuitInConstants [
 
	self addCode: (ForthConstant on: forth name: 'version' value: 7).
	self addCode: (ForthConstant on: forth name: 'docol' value: (forth labels at: #DOCOL)).
	self addCode: (ForthConstant on: forth name: 'f_lenmask' value: forth flagLengthMask).
	self addCode: (ForthConstant on: forth name: 'f_hidden' value: forth flagHidden).
	self addCode: (ForthConstant on: forth name: 'f_immed' value: forth flagImmediate).
	self addCode: (ForthConstant on: forth name: 'r0' value: forth memory r0).

]
