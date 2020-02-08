Extension { #name : #Integer }

{ #category : #'*Phortissimo-Core' }
Integer >> asSigned32 [


	^ self > 16r7FFFFFFF 
		ifTrue: [ (self bitInvert32 + 1) negated ]
		ifFalse: [ 
			self < 0 
				ifTrue: [ self error: 'number is signed already' ]
				ifFalse: [ self ] ]
]

{ #category : #'*Phortissimo-Core' }
Integer >> asUnsigned32 [

	^ self < 0
		ifFalse: [ self bitAnd: 16rFFFFFFFF ]
		ifTrue: [ ((self - 1) bitInvert32 negated bitAnd: 16rFFFFFFFF) ].
	
]

{ #category : #'*Phortissimo-Core' }
Integer >> asUnsigned4Bytes [

	| num |
	
	num := self < 0
		ifFalse: [ self bitAnd: 16rFFFFFFFF ]
		ifTrue: [ ((self - 1) bitInvert32 negated bitAnd: 16rFFFFFFFF) ].
	
	^ num asByteArrayOfSize: 4.
]
