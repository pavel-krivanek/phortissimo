Class {
	#name : #ForthCodeQuit,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Core-Code-Interpret'
}

{ #category : #accessing }
ForthCodeQuit >> codewordLabels [

	"RZ RSPSTORE"
	^ #(INTERPRET BRANCH -8)
]

{ #category : #accessing }
ForthCodeQuit >> name [

	^ 'quit'
]
