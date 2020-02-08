Class {
	#name : #ForthCodeRSPFetch,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Return stack'
}

{ #category : #accessing }
ForthCodeRSPFetch >> execute [ 

	forth push: (forth returnStack size // 4) asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodeRSPFetch >> name [

	^ 'rsp@'
]
