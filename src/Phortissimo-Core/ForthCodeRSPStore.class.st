Class {
	#name : #ForthCodeRSPStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeRSPStore >> execute [ 

	forth memory rsp: forth pop asUnsigned32
]

{ #category : #accessing }
ForthCodeRSPStore >> name [

	^ 'rsp!'
]
