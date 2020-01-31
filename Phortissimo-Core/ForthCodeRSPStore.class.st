Class {
	#name : #ForthCodeRSPStore,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeRSPStore >> execute [ 

	self shouldBeImplemented
]

{ #category : #accessing }
ForthCodeRSPStore >> name [

	^ 'rsp!'
]
