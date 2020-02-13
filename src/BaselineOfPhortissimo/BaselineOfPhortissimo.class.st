Class {
	#name : #BaselineOfPhortissimo,
	#superclass : #BaselineOf,
	#category : #BaselineOfPhortissimo
}

{ #category : #baselines }
BaselineOfPhortissimo >> baseline: spec [
	<baseline>
	
	spec for: #common do: [
		spec 
			package: 'Phortissimo-Core';
			package: 'Phortissimo-Tests' ]
]
