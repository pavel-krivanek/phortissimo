Class {
	#name : #ForthCodePenBusy,
	#superclass : #ForthCodeWithHead,
	#category : #'Phortissimo-Core-Code-Plotter'
}

{ #category : #accessing }
ForthCodePenBusy >> execute [ 

	forth push: forth plotter busy asBit asUnsigned4Bytes
]

{ #category : #accessing }
ForthCodePenBusy >> name [

	^ 'penBusy'
]
