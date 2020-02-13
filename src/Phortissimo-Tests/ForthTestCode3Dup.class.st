Class {
	#name : #ForthTestCode3Dup,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Tests'
}

{ #category : #accessing }
ForthTestCode3Dup >> codewordLabels [

	^ #(DUP DUP DUP EXIT)
]

{ #category : #accessing }
ForthTestCode3Dup >> label [

	^ '3DUP'
]

{ #category : #accessing }
ForthTestCode3Dup >> name [

	^ '3dup'
]
