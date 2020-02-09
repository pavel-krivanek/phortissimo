Class {
	#name : #ForthCodeToR,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeToR >> execute [ 

	forth returnStackPush: forth pop
]

{ #category : #accessing }
ForthCodeToR >> name [

	^ '>r'
]
