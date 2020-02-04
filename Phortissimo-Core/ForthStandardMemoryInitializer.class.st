Class {
	#name : #ForthStandardMemoryInitializer,
	#superclass : #ForthMemoryInitializer,
	#category : #'Phortissimo-Core-Basic-Memory initialization'
}

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeBasicPrimitives [

	self installAll: { 
		ForthCodeDrop.
		ForthCodeSwap.
		ForthCodeDup.
		ForthCodeOver.
		ForthCodeRot.
		ForthCodeNRot.
		ForthCodeTwoDrop.
		ForthCodeTwoDup.
		ForthCodeTwoSwap.
		ForthCodeQDup.
		ForthCodeIncr.
		ForthCodeDecr.
		ForthCodeIncr4.
		ForthCodeDecr4.
		ForthCodeAdd.
		ForthCodeSub.
		ForthCodeMul.
		ForthCodeDivMod. 
	}
	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeBitwisePrimitives [

	self installAll: { 
		ForthCodeAnd.
		ForthCodeOr.
		ForthCodeXor.
		ForthCodeInvert.
	}

]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeBranchingPrimitives [

	self installAll: { 
		ForthCodeBranch.
		ForthCodeZBranch.
	}

]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeBuitInConstants [
 
	self addCode: (ForthConstant on: forth name: 'VERSION' value: 7).
	self addCode: (ForthConstant on: forth name: 'DOCOL' value: (forth labels at: #DOCOL)).
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeBuitInVarialbes [

	self addCode: (ForthVariable on: forth name: 'state').
	self addCode: (ForthVariable on: forth name: 'here').
	self addCode: (ForthVariable on: forth name: 'latest' initialValue: nil).
	self addCode: (ForthVariable on: forth name: 's0' initialValue: 1).
	self addCode: (ForthVariable on: forth name: 'base' initialValue: 10).
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeComparisonPrimitives [

	self installAll: { 
		ForthCodeEqu.
		ForthCodeNEqu.
		ForthCodeLT.
		ForthCodeGT.
		ForthCodeLE.
		ForthCodeGE.
		ForthCodeZEqu.
		ForthCodeZNEqu.
		ForthCodeZLT.
		ForthCodeZGT.
		ForthCodeZLE.
		ForthCodeZGE.
	}

]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeCompilerExtendingPrimitives [

	self installAll: { 
		ForthCodeImmediate.
		ForthCodeHidden.
		ForthCodeHide.
		ForthCodeTick.
	}
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeCompilingPrimitives [

	self installAll: { 
		ForthCodeCreate.
		ForthCodeComma.
		ForthCodeLBrac.
		ForthCodeRBrac.
		ForthCodeColon.
		ForthCodeSemicolon.
	}

]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeDataStackPrimitives [

	self installAll: { 
		ForthCodeDSPFetch.
		ForthCodeDSPStore.
	}
	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeDictionaryLookupPrimitives [

	self installAll: { 
		ForthCodeFind.
		ForthCodeTCFA.
		ForthCodeTDFA.
	}
		
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeExit [

	self installAll: { 
		ForthCodeExit.
	}

]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeIOPrimitives [

	self installAll: { 
		ForthCodeKey.
		ForthCodeEmit.
	}
	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeInterpreterPrimitives [

	self installAll: { 
		ForthCodeHalt.
		ForthCodeQuit.
		ForthCodeInterpret.
	}
	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeLiteralPrimitives [

	self installAll: { 
		ForthCodeLit.
	}

	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeLiteralsPrimitives [

	self installAll: { 
		ForthCodeLit.
	}
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeMemory [
	
	initPos := 1.
	
	1 to: forth memory size do: [ :i | forth memory at: i put: 0 ].
	
	self addCode: (ForthCodeDoCol on: forth).
	self addCode: (ForthCodeNext on: forth).

	self initializeBasicPrimitives.
	self initializeComparisonPrimitives.
	self initializeBitwisePrimitives.
	self initializeLiteralPrimitives.
	self initializeMemoryPrimitives.
	self initializeExit.

	self initializeBuitInVarialbes.
	self initializeBuitInConstants.
	
	self initializeReturnStackPrimitives.
	self initializeDataStackPrimitives.
	self initializeIOPrimitives.
	self initializeParsingPrimitives.
	self initializeDictionaryLookupPrimitives.	
	self initializeCompilingPrimitives.	
	self initializeCompilerExtendingPrimitives.
	self initializeBranchingPrimitives.
	self initializeStringLiteralsPrimitives.
	self initializeInterpreterPrimitives.
		
	forth varLatestValue: (forth addressForLabel: 'name_INTERPRET').
	forth varHereValue: initPos.
	
	forth fixUnknownLabels.

	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeMemoryPrimitives [

	self installAll: {
		ForthCodeStore.
		ForthCodeFetch.
		ForthCodeAddStore.
		ForthCodeSubStore.
	
		ForthCodeStoreByte.
		ForthCodeFetchByte.
		ForthCodeCCopy.
		ForthCodeCMove.
	}

	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeParsingPrimitives [

	self installAll: { 
		ForthCodeWord.
		ForthCodeNumber.
	}
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeReturnStackPrimitives [

	self installAll: { 
		ForthCodeToR.
		ForthCodeFromR.
		ForthCodeRSPFetch.
		ForthCodeRSPStore.
		ForthCodeRDrop.
	}
	
]

{ #category : #initialization }
ForthStandardMemoryInitializer >> initializeStringLiteralsPrimitives [

	self installAll: { 
		ForthCodeLitString.
		ForthCodeTell.
		ForthCodeChar.
	}
]
