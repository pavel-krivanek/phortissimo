Class {
	#name : #ForthCodeRSPFetch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeRSPFetch >> execute [ 

	forth push: (forth memory rsp) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeRSPFetch >> name [

	^ 'rsp@'
]
