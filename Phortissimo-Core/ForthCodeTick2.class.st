Class {
	#name : #ForthCodeTick2,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Core-Code-Compiler extending'
}

{ #category : #accessing }
ForthCodeTick2 >> codewordLabels [

	^ #(WORD FIND TCFA EXIT)
]

{ #category : #accessing }
ForthCodeTick2 >> name [

	^ ''''
]
