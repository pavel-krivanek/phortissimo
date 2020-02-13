Class {
	#name : #ForthTest7DupEnvironment,
	#superclass : #Forth,
	#category : #'Phortissimo-Tests'
}

{ #category : #initialization }
ForthTest7DupEnvironment >> memoryInitializer [

	^ ForthTest7DupMemoryInitializer for: self
]
