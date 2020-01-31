Class {
	#name : #ForthCodeColon,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Core-Code-Compiling'
}

{ #category : #accessing }
ForthCodeColon >> codewordLabels [

"	^ #(WORD CREATE LIT DOCOL COMMA LATEST FETCH HIDDEN RBRAC EXIT)
"	
	^ #(WORD CREATE LIT DOCOL COMMA LATEST FETCH HIDDEN RBRAC EXIT)
]

{ #category : #accessing }
ForthCodeColon >> name [

	^ ':'
]
