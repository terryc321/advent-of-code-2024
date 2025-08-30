
"to:do"

"exactly how to:do is implemented in Number"
"1 to: 100 do: [:i | Transcript show: i ; cr ]. "
"this is the code in the IDE GUI , not sure how encode this in text"

too: stop do: aBlock 
	"Normally compiled in-line, and therefore not overridable.
	Evaluate aBlock for each element of the interval (self to: stop by: 1)."
	| nextValue |
	nextValue := self.
	[nextValue <= stop]
		whileTrue: 
			[aBlock value: nextValue.
			nextValue := nextValue + 1] 	
		
			
Transcript clear.		
1 too: 100 do: [:i | Transcript show: i ; cr].		

repeatedly bangs on aBlock until whileTrue condition is false