Class {
	#name : #ForthTestStd,
	#superclass : #ForthTest,
	#category : #'Phortissimo-Tests'
}

{ #category : #tests }
ForthTestStd >> testBases [

	forth := ForthUppercase new init.
	forth loadStd.
	forth << '
	: TEST
		123                                . CR
		[ HEX -7F ] LITERAL      DECIMAL   . CR
		[ HEX 7FF77FF7 ] LITERAL HEX       . CR
		[ HEX -7FF77FF7 ] LITERAL 2 BASE ! . CR
		[ 2 BASE ! 1111111111101110111111111110111 ] LITERAL HEX . CR
	;
DECIMAL
TEST'.

	forth run.
	self assert: (forth outputBufferString copyReplaceAll: String lf with: String cr) equals: '123 
-127 
7FF77FF7 
-1111111111101110111111111110111 
7FF77FF7 
'


]

{ #category : #tests }
ForthTestStd >> testComparison [

	forth := ForthUppercase new init.
	forth loadStd.
	forth << ': TEST
	1 0 < . CR
	0 1 < . CR
	1 -1 < . CR
	-1 1 < . CR
	-1 0 < . CR
	0 -1 < . CR

	1 0 > . CR
	0 1 > . CR
	1 -1 > . CR
	-1 1 > . CR
	-1 0 > . CR
	0 -1 > . CR

	1 1 <= . CR
	0 0 <= . CR
	-1 -1 <= . CR
	1 0 <= . CR
	0 1 <= . CR
	1 -1 <= . CR
	-1 1 <= . CR
	-1 0 <= . CR
	0 -1 <= . CR

	1 1 >= . CR
	0 0 >= . CR
	-1 -1 >= . CR
	1 0 >= . CR
	0 1 >= . CR
	1 -1 >= . CR
	-1 1 >= . CR
	-1 0 >= . CR
	0 -1 >= . CR

	1 1 = . CR
	1 0 = . CR
	0 0 = . CR
	1 -1 = . CR
	-1 -1 = . CR

	1 1 <> . CR
	1 0 <> . CR
	0 0 <> . CR
	1 -1 <> . CR
	-1 -1 <> . CR

	1 0= . CR
	0 0= . CR
	-1 0= . CR

	1 0<> . CR
	0 0<> . CR
	-1 0<> . CR

	1 0< . CR
	0 0< . CR
	-1 0< . CR

	1 0> . CR
	0 0> . CR
	-1 0> . CR

	1 0<= . CR
	0 0<= . CR
	-1 0<= . CR

	1 0>= . CR
	0 0>= . CR
	-1 0>= . CR
;
TEST'.

	forth run.
	self assert: (forth outputBufferString withSeparatorsCompacted ) equals: '0 1 0 1 1 0 1 0 1 0 0 1 1 1 1 0 1 0 1 1 0 1 1 1 1 0 1 0 0 1 1 0 1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 0 1 1 0 0 0 1 1 1 1 0 '


]

{ #category : #tests }
ForthTestStd >> testStack [

	forth := ForthUppercase new init.
	forth loadStd.
	forth << ': TEST
	DEPTH . CR

	42 DUP . . CR
	23 DROP DEPTH . CR
	1 2 SWAP . . CR
	1 2 OVER . . . CR
	1 2 3 -ROT . . . CR
	1 2 3 ROT . . . CR
	1 2 3 4 2DROP . . CR
	1 2 3 4 2DUP . . . . . . CR
	1 2 3 4 2SWAP . . . . CR

	DEPTH . CR
;
TEST'.

	forth run.
	self assert: (forth outputBufferString withSeparatorsCompacted ) equals: '0 42 42 0 1 2 1 2 1 2 1 3 1 3 2 2 1 4 3 4 3 2 1 2 1 4 3 0 '

]
