'From Cuis7.5 [latest update: #7471] on 31 August 2025 at 12:21:05 am'!
'Description a variation on a normal array
'!
!provides: 'Tweak-Array' 1 6!
SystemOrganization addCategory: #'Tweak-Array'!


!classDefinition: #TArray category: #'Tweak-Array'!
Array variableSubclass: #TArray
	instanceVariableNames: 'arr'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tweak-Array'!
!classDefinition: 'TArray class' category: #'Tweak-Array'!
TArray class
	instanceVariableNames: ''!


!TArray commentStamp: '<historical>' prior: 0!
can we make our own Array class that implements safe? and constructor method
!

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:48:10'!
differBy: a with: b
	"two values differ by 1 2 or 3"
	|diff|
	diff := (a - b) abs.
	diff = 1 ifTrue: [^true].
	diff = 2 ifTrue: [^true].
	diff = 3 ifTrue: [^true].
	^false.
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:59:30'!
from: aArray
	"a plain copy of the array?"
       |sz |
	sz := aArray size.
	arr := Array new: sz .
	1 to: sz do: [ :i | arr at: i put: (aArray at: i) ].
	
	"Transcript cr ; show: 'we created a new array from...' ; show: arr ; cr . "
	^ self.
	
	
	
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:54:12'!
initialize
	"new from"
	arr := nil.
	"Transcript show: 'we initialized arr to nil...'."
	
	
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:43:22'!
isDecreasing
	"are all values decreasing?"
	|sz|
	sz := arr size.
	1 to: sz do: [:i | 
		|prev next|
		prev := arr at: i.
		i < sz ifTrue:[ next := arr at: (i + 1) ] ifFalse: [ next := nil ].
		next = nil ifTrue:[ ^ true] ifFalse:[next < prev ifFalse:[^ false] ]
		].
	^ true.
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:53:25'!
isDecreasingBy1to3
	"are all values increasing?"
	|sz|
	sz := arr size.
	1 to: sz do: [:i | 
		|prev next|
		prev := arr at: i.
		i < sz ifTrue:[ next := arr at: (i + 1) ] ifFalse: [ next := nil ].
		next = nil ifTrue:[ ^ true] ifFalse:[
			|diff|
			diff := self differBy: next with: prev.
			(next < prev and: [diff ]) ifFalse:[^ false] 
			].
		].
	^ true.
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 02:07:30'!
isDecreasingBy1to3bad1
	"are all values decreasing?"
	|sz bad|
	bad := 0 . "no bad intervals yet".
	sz := arr size.
	1 to: sz do: [:i | 
		|prev next|
		prev := arr at: i.
		i < sz ifTrue:[ next := arr at: (i + 1) ] ifFalse: [ next := nil ].
		next = nil ifTrue:[ ^ true] ifFalse:[
			|diff|
			diff := self differBy: next with: prev.
			(next < prev and: [diff ]) ifFalse:[bad := bad + 1 ] .
			bad > 1 ifTrue:[^false].
			].
		].
	^ true.
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:41:58'!
isIncreasing
	"are all values increasing?"
	|sz|
	sz := arr size.
	1 to: sz do: [:i | 
		|prev next|
		prev := arr at: i.
		i < sz ifTrue:[ next := arr at: (i + 1) ] ifFalse: [ next := nil ].
		next = nil ifTrue:[ ^ true] ifFalse:[next > prev ifFalse:[^ false] ]
		].
	^ true.
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:52:07'!
isIncreasingBy1to3
	"are all values increasing?"
	|sz|
	sz := arr size.
	1 to: sz do: [:i | 
		|prev next|
		prev := arr at: i.
		i < sz ifTrue:[ next := arr at: (i + 1) ] ifFalse: [ next := nil ].
		next = nil ifTrue:[ ^ true] ifFalse:[
			|diff|
			diff := self differBy: next with: prev.
			(next > prev and: [diff ]) ifFalse:[^ false] 
			].
		].
	^ true.
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 02:08:07'!
isIncreasingBy1to3bad1
	"are all values increasing?"
	|sz bad|
	bad := 0 . "no bad values".
	sz := arr size.
	1 to: sz do: [:i | 
		|prev next|
		prev := arr at: i.
		i < sz ifTrue:[ next := arr at: (i + 1) ] ifFalse: [ next := nil ].
		next = nil ifTrue:[ ^ true] ifFalse:[
			|diff|
			diff := self differBy: next with: prev.
			(next > prev and: [diff ]) ifFalse:[ bad := bad + 1 ] .
			bad > 1 ifTrue:[^ false].
			].
		].
	^ true.
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 01:58:15'!
isSafe
	"safe if isDecreasingBy1to3 or isIncreasingBy1to3"
	^ (self isIncreasingBy1to3) or: (self isDecreasingBy1to3) .
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/31/2025 00:00:46'!
isSafeBad1
   "essentially if we remove one element from the array then ask isSafe 
we should get correct answer , provided isSafe works correctly.
arr is local variable to class Tarray is just a normal array
figure out how to go about removing the nth element of an array -> make a new array with that element removed
construct new TArray object and call isSafe on it , if it is safe then isSafeBad1 is also safe.
possibly duplicating copyign going on but for this quick and dirty
"
      
	| sz |
	sz := arr size.
	1 to: sz do: [ :i |
		|tmp |
		tmp := Array new: (sz-1).		
		1 to: sz do: [:j | 
			j < i ifTrue:[].
			"j = i ifTrue:[]. skip"
			j > i ifTrue:[tmp at: (j-1) put: 0].
			].
		^ tmp.
		].
	"check if it is safe - if so , return true - it is safe with one bad element removed"
	^ true. 
	
	! !

!TArray methodsFor: 'as yet unclassified' stamp: 'tc 8/31/2025 00:20:58'!
remove: item
  "remove the nth item from self
  if item is larger than size of array then method is ignored ? or should throw error? 
  lets just return size of array for now
 iterate over the array itself , add items if i not = item then end up with either original array or smaller array
"
   | sz oc oc2 |
    sz := arr size. 
    oc := #() asOrderedCollection .

   1 to: sz do: [ :elem | 
		elem = item ifTrue:[ nil ] ifFalse:[ oc add: (arr at: elem ) ] 
	].
   oc2 := oc asArray.
   Transcript show: oc2 ; cr . 
   ^ TArray from: oc2 





! !
