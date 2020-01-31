Class {
	#name : #ForthTest7DupMemoryInitializer,
	#superclass : #ForthMemoryInitializer,
	#category : #'Phortissimo-Tests'
}

{ #category : #initialization }
ForthTest7DupMemoryInitializer >> initializeMemory [
	
	initPos := 1.
	
	self addCode: (ForthCodeDoCol on: forth).
	self addCode: (ForthCodeNext on: forth).
	self addCode: (ForthCodeExit on: forth).

	self addCode: (ForthCodeDup on: forth).
	self addCode: (ForthTestCode3Dup on: forth).
	self addCode: (ForthTestCode7Dup on: forth).

	
]
