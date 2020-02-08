Class {
	#name : #ForthCodeCreate,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Compiling'
}

{ #category : #accessing }
ForthCodeCreate >> execute [ 

	| length nameAddress header latest |
	
	length := forth pop asSigned32.
	nameAddress := forth pop asUnsigned32.
	header := forth varHereValue.
	latest := forth varLatestValue.
	forth writeWord: latest at: header.
	forth writeByte: length at: header + forth wordSize.
	1 to: length do: [ :i | 
		forth writeByte: (forth byteAt: nameAddress + i - 1) at: header + forth wordSize + i ].

	forth varLatestValue: header.
	forth varHereValue: (header + forth wordSize + length + 1).
]

{ #category : #accessing }
ForthCodeCreate >> name [

	^ 'create'
]
