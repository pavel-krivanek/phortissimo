Class {
	#name : #ForthTestCode7Dup,
	#superclass : #ForthCodeWithHeadCompiled,
	#category : #'Phortissimo-Tests'
}

{ #category : #accessing }
ForthTestCode7Dup >> codewordLabels [

	^ #('3DUP' '3DUP' DUP EXIT)
]

{ #category : #accessing }
ForthTestCode7Dup >> label [

	^ '7DUP'
]

{ #category : #accessing }
ForthTestCode7Dup >> name [

	^ '7dup'
]
