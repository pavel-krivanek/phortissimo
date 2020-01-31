Class {
	#name : #ForthCodeHide,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Core-Code-Compiler extending'
}

{ #category : #accessing }
ForthCodeHide >> codewordLabels [

	^ #(WORD FIND HIDDEN EXIT)
]

{ #category : #accessing }
ForthCodeHide >> name [

	^ 'hide'
]
