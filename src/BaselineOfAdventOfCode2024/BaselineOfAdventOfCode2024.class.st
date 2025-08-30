Class {
	#name : #BaselineOfAdventOfCode2024,
	#superclass : #BaselineOf,
	#category : #BaselineOfAdventOfCode2024
}

{ #category : #baseline }
BaselineOfAdventOfCode2024 >> baseline: spec [
	<baseline>
		^ spec for: #common do: [
			spec package: 'AdventOfCode2024'
		]
]
