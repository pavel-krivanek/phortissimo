Class {
	#name : #ForthCodeSemicolon,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Core-Code-Compiling'
}

{ #category : #accessing }
ForthCodeSemicolon >> codewordLabels [

	^ #(LIT EXIT COMMA LATEST FETCH HIDDEN LBRAC EXIT)
]

{ #category : #accessing }
ForthCodeSemicolon >> flags [ 

	^ forth flagImmediate 
]

{ #category : #accessing }
ForthCodeSemicolon >> name [

	^ ';'
]
