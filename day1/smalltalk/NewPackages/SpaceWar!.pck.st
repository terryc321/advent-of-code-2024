'From Cuis7.5 [latest update: #7452] on 30 August 2025 at 3:22:08 pm'!
'Description '!
!provides: 'SpaceWar!!' 1 30!
SystemOrganization addCategory: #'SpaceWar!!'!


!classDefinition: #CasetteMenu category: #'SpaceWar!!'!
PlacedMorph subclass: #CasetteMenu
	instanceVariableNames: 'inspectFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'CasetteMenu class' category: #'SpaceWar!!'!
CasetteMenu class
	instanceVariableNames: ''!

!classDefinition: #CentralStar category: #'SpaceWar!!'!
PlacedMorph subclass: #CentralStar
	instanceVariableNames: 'velocity position acceleration fColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'CentralStar class' category: #'SpaceWar!!'!
CentralStar class
	instanceVariableNames: ''!

!classDefinition: #ForwardButton category: #'SpaceWar!!'!
PlacedMorph subclass: #ForwardButton
	instanceVariableNames: 'inspectFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'ForwardButton class' category: #'SpaceWar!!'!
ForwardButton class
	instanceVariableNames: ''!

!classDefinition: #PauseButton category: #'SpaceWar!!'!
PlacedMorph subclass: #PauseButton
	instanceVariableNames: 'inspectFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'PauseButton class' category: #'SpaceWar!!'!
PauseButton class
	instanceVariableNames: ''!

!classDefinition: #Planet category: #'SpaceWar!!'!
PlacedMorph subclass: #Planet
	instanceVariableNames: 'velocity position acceleration fColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'Planet class' category: #'SpaceWar!!'!
Planet class
	instanceVariableNames: 'position velocity acceleration'!

!classDefinition: #PlayButton category: #'SpaceWar!!'!
PlacedMorph subclass: #PlayButton
	instanceVariableNames: 'inspectFlag dashOffset aStart aEnd aControlPoint bStart bEnd bControlPoint1 bControlPoint2 tick'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'PlayButton class' category: #'SpaceWar!!'!
PlayButton class
	instanceVariableNames: ''!

!classDefinition: #ReverseButton category: #'SpaceWar!!'!
PlacedMorph subclass: #ReverseButton
	instanceVariableNames: 'inspectFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'ReverseButton class' category: #'SpaceWar!!'!
ReverseButton class
	instanceVariableNames: ''!

!classDefinition: #SpaceWar category: #'SpaceWar!!'!
PlacedMorph subclass: #SpaceWar
	instanceVariableNames: 'inspectFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'SpaceWar class' category: #'SpaceWar!!'!
SpaceWar class
	instanceVariableNames: ''!

!classDefinition: #StopButton2 category: #'SpaceWar!!'!
PlacedMorph subclass: #StopButton2
	instanceVariableNames: 'inspectFlag'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'StopButton2 class' category: #'SpaceWar!!'!
StopButton2 class
	instanceVariableNames: ''!

!classDefinition: #Torpedo category: #'SpaceWar!!'!
PlacedMorph subclass: #Torpedo
	instanceVariableNames: 'velocity position acceleration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'Torpedo class' category: #'SpaceWar!!'!
Torpedo class
	instanceVariableNames: ''!


!CasetteMenu commentStamp: '<historical>' prior: 0!
1) save using packages
2) never save the image
3) file out 
4) start cuis in code directory


overall collection of classes


|c w |
Transcript clear.
c := CentralStar new.
c openInWorld.
w := c world. 
w submorphs do: [:object | 
	Transcript show: 'The object name: ' , object className; cr.
	"object className = 'CentralStar' ifTrue: [object delete] "
	object = CentralStar ifTrue: [object delete] 
	].
c delete.

!

!CentralStar commentStamp: '<historical>' prior: 0!
velocity position and acceleration!

!ForwardButton commentStamp: '<historical>' prior: 0!
1) save using packages
2) never save the image
3) file out 
4) start cuis in code directory


overall collection of classes


|c w |
Transcript clear.
c := CentralStar new.
c openInWorld.
w := c world. 
w submorphs do: [:object | 
	Transcript show: 'The object name: ' , object className; cr.
	"object className = 'CentralStar' ifTrue: [object delete] "
	object = CentralStar ifTrue: [object delete] 
	].
c delete.

!

!PauseButton commentStamp: '<historical>' prior: 0!
1) save using packages
2) never save the image
3) file out 
4) start cuis in code directory


overall collection of classes


|c w |
Transcript clear.
c := CentralStar new.
c openInWorld.
w := c world. 
w submorphs do: [:object | 
	Transcript show: 'The object name: ' , object className; cr.
	"object className = 'CentralStar' ifTrue: [object delete] "
	object = CentralStar ifTrue: [object delete] 
	].
c delete.

!

!Planet commentStamp: '<historical>' prior: 0!
velocity position and acceleration!

!PlayButton commentStamp: '<historical>' prior: 0!
this is a comment for play buttons!

!ReverseButton commentStamp: '<historical>' prior: 0!
1) save using packages
2) never save the image
3) file out 
4) start cuis in code directory


overall collection of classes


|c w |
Transcript clear.
c := CentralStar new.
c openInWorld.
w := c world. 
w submorphs do: [:object | 
	Transcript show: 'The object name: ' , object className; cr.
	"object className = 'CentralStar' ifTrue: [object delete] "
	object = CentralStar ifTrue: [object delete] 
	].
c delete.

!

!SpaceWar commentStamp: '<historical>' prior: 0!
1) save using packages
2) never save the image
3) file out 
4) start cuis in code directory


overall collection of classes


|c w |
Transcript clear.
c := CentralStar new.
c openInWorld.
w := c world. 
w submorphs do: [:object | 
	Transcript show: 'The object name: ' , object className; cr.
	"object className = 'CentralStar' ifTrue: [object delete] "
	object = CentralStar ifTrue: [object delete] 
	].
c delete.

!

!StopButton2 commentStamp: '<historical>' prior: 0!
1) save using packages
2) never save the image
3) file out 
4) start cuis in code directory


overall collection of classes


|c w |
Transcript clear.
c := CentralStar new.
c openInWorld.
w := c world. 
w submorphs do: [:object | 
	Transcript show: 'The object name: ' , object className; cr.
	"object className = 'CentralStar' ifTrue: [object delete] "
	object = CentralStar ifTrue: [object delete] 
	].
c delete.

!

!Torpedo commentStamp: '<historical>' prior: 0!
velocity position and acceleration!

!CasetteMenu methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:17'!
drawOn: aCanvas
   inspectFlag ifTrue:[ aCanvas inspect. 
	                            inspectFlag := false ].
"   aCanvas
      ellipseCenter: 0@0
      radius: 100
      borderWidth: 10
      borderColor: Color lightCyan
      fillColor: Color veryVeryLightGray.
"

   aCanvas drawString: 'XII' at: -13 @ -90 font: nil color: Color brown.
   aCanvas drawString: 'III' at: 66 @ -10 font: nil color: Color brown.
   aCanvas drawString: 'VI' at: -11 @ 70 font: nil color: Color brown.
   aCanvas drawString: 'IX' at: -90 @ -10 font: nil color: Color brown.


! !

!CasetteMenu methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:17'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
   inspectFlag := true.


! !

!CasetteMenu methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:17'!
stepAt: millisecondSinceLast
   "do some update"


! !

!CasetteMenu methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:17'!
stepTime
  " milliseconds  "
  ^ 20 .

! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 14:00:02'!
attract
  " do something "
  "self changed."
  "Transcript show: 'tick' ; cr ."
    Planet allInstancesDo: [ :planet | 
	|sunPos planetPos diffPos stepPos|
		"Transcript show: 'Planet = ' ; show: planet ; cr . "			
		sunPos := self morphPosition.
		planetPos := planet morphPosition.
		diffPos := sunPos - planetPos.
		stepPos := diffPos / 100 . 
		planet morphPosition: (planetPos + stepPos).		
	]. 
  

   self redrawNeeded.




! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 15:16:30'!
drawOn: canvas
   | radius above |
   radius := self morphExtent // 2.
   canvas ellipseCenter: 0 @ 0
      radius: (radius x + (5 atRandom - 2.5)) @ (radius y + (5 atRandom - 2.5))
      borderWidth: 3 
      borderColor: Color orange 
     "  fillColor: Color random."
       fillColor: Color yellow .
      "self changed."

    above := 100 .
    canvas strokeWidth: 1 color: `Color blue` do: [
	       "border 100 100 rectangle"
		canvas
				moveTo: 0@ above;
				lineTo: 238@ above.
          ].

     canvas drawString: 'Sun' at: -30@above  font: nil  color: Color blue.
     canvas drawString: 'Earth' at: (238)@above  font: nil  color: Color blue.
! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:15:03'!
initialize
   super initialize.
   velocity := 0 @ 0.
   position := 100 @ 100.
   acceleration := 0.   
   fColor := Color blue .
! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:05:51'!
morphExtent
   ^ `200 @ 200`

! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 14:00:18'!
step
  " do something "
  "self changed."
  "Transcript show: 'tick' ; cr ."
   self attract.
   self redrawNeeded.




! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:18:41'!
stepTime
  " milliseconds  "
  ^ 200 .
! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:11:15'!
wantsSteps
  " animate me? "
  ^ true .
! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 15:04:06'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true! !

!ForwardButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:29'!
drawOn: aCanvas
   inspectFlag ifTrue:[ aCanvas inspect. 
	                            inspectFlag := false ].
"   aCanvas
      ellipseCenter: 0@0
      radius: 100
      borderWidth: 10
      borderColor: Color lightCyan
      fillColor: Color veryVeryLightGray.
"

   aCanvas drawString: 'XII' at: -13 @ -90 font: nil color: Color brown.
   aCanvas drawString: 'III' at: 66 @ -10 font: nil color: Color brown.
   aCanvas drawString: 'VI' at: -11 @ 70 font: nil color: Color brown.
   aCanvas drawString: 'IX' at: -90 @ -10 font: nil color: Color brown.


! !

!ForwardButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:29'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
   inspectFlag := true.


! !

!ForwardButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:29'!
stepAt: millisecondSinceLast
   "do some update"


! !

!ForwardButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:29'!
stepTime
  " milliseconds  "
  ^ 20 .

! !

!PauseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:36'!
drawOn: aCanvas
   inspectFlag ifTrue:[ aCanvas inspect. 
	                            inspectFlag := false ].
"   aCanvas
      ellipseCenter: 0@0
      radius: 100
      borderWidth: 10
      borderColor: Color lightCyan
      fillColor: Color veryVeryLightGray.
"

   aCanvas drawString: 'XII' at: -13 @ -90 font: nil color: Color brown.
   aCanvas drawString: 'III' at: 66 @ -10 font: nil color: Color brown.
   aCanvas drawString: 'VI' at: -11 @ 70 font: nil color: Color brown.
   aCanvas drawString: 'IX' at: -90 @ -10 font: nil color: Color brown.


! !

!PauseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:36'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
   inspectFlag := true.


! !

!PauseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:36'!
stepAt: millisecondSinceLast
   "do some update"


! !

!PauseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:26:36'!
stepTime
  " milliseconds  "
  ^ 20 .

! !

!Planet methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:07:54'!
drawOn: canvas
   | radius |
   radius := self morphExtent // 2.
   canvas ellipseCenter: 0 @ 0
      radius: (radius x + (5 atRandom - 1)) @ (radius y + (5 atRandom - 1))
      borderWidth: 3 
      borderColor: Color orange 
      fillColor: Color blue.
      "self changed."

! !

!Planet methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:04:45'!
initialize
   super initialize.
   velocity := 0 @ 0.
   position := 100 @ 100.
   acceleration := 0.   
   fColor := Color blue .
! !

!Planet methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:04:45'!
morphExtent
   ^ `50 @ 50`

! !

!Planet methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:04:45'!
step
  " do something "
  "self changed."
  "Transcript show: 'tick' ; cr ."
   self redrawNeeded.




! !

!Planet methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:04:45'!
stepTime
  " milliseconds  "
  ^ 200 .
! !

!Planet methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 13:05:02'!
wantsSteps
  " animate me? "
  ^ false .
! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 12:03:05'!
drawOn: aCanvas

	aCanvas fillColor: `Color green` do: [
		"aCanvas circleCenter: `30@60` radius: 150."
		aCanvas ellipseCenter: `30@60` radius: 100@50.
     ].

"	aCanvas fillColor: `Color green` do: [
		aCanvas circleCenter: `30@60` radius: 15.
		aCanvas
			moveTo: `0@0`;
			lineTo: `100@ -50`;
			arcTo: `100@ 50` radius: 110 angleOfXAxis: 0 largeFlag: true sweepFlag: true;
			lineTo: `0@0`.
		].
"
	
aCanvas strokeWidth: 10 color: `Color blue` do: [
	       "border 100 100 rectangle"
		aCanvas
				moveTo: `0@0`;
				lineTo: `100@0`;		
				lineTo: `100@100`;		
				lineTo: `0@100`;		
				lineTo: `0@0`.				
              "inner box 50 50 rectangle"				
		aCanvas
				moveTo: `25@25`;
				lineTo: `75@25`;		
				lineTo: `75@75`;		
				lineTo: `25@75`;		
				lineTo: `25@25`.				
		].

	! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 12:50:49'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
"   inspectFlag := true. inspectFlag not used"
"super initialize. what happens if initialize twice"
	aStart := 500 atRandom @ 500 atRandom.
	aEnd := 500 atRandom @ 500 atRandom.
	aControlPoint := 500 atRandom @ 500 atRandom.
	bStart := 500 atRandom @ 500 atRandom.
	bEnd := 500 atRandom @ 500 atRandom.
	bControlPoint1 := 500 atRandom @ 500 atRandom.
	bControlPoint2 := 500 atRandom @ 500 atRandom.
	dashOffset := 0.
	tick := 0.
	 self startStepping .
	
	
	

! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:58:47'!
rotationCenter
	"Smooth rotation around the origin.
	(Optional. See what happens if you delete this method)"

	^0@0! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 12:51:23'!
step
        "redraw needed causes system to redraw the canvas causing animation to take effect"	

       "super stepAt: millisecondSinceLast."
	"self updateContents."
	dashOffset := dashOffset + 1 .
	dashOffset > 10 ifTrue:[dashOffset := 0].
	"millisecondSinceLast > 200 ifTrue:[self redrawNeeded ]."
	tick := tick + 1 .	
	Transcript show: 'tick = ' ; show: tick ; cr . 
	self redrawNeeded .
	
	
	
	
	
	
	
	
! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 10:19:03'!
stepTime
  " milliseconds  "
  ^ 200 .

! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 10:06:42'!
wantsSteps
    " if want this to be animated - wantsSteps has to return true ^true"
	^ true! !

!PlayButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:58:33'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^true! !

!ReverseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:38'!
drawOn: aCanvas
   inspectFlag ifTrue:[ aCanvas inspect. 
	                            inspectFlag := false ].
"   aCanvas
      ellipseCenter: 0@0
      radius: 100
      borderWidth: 10
      borderColor: Color lightCyan
      fillColor: Color veryVeryLightGray.
"

   aCanvas drawString: 'XII' at: -13 @ -90 font: nil color: Color brown.
   aCanvas drawString: 'III' at: 66 @ -10 font: nil color: Color brown.
   aCanvas drawString: 'VI' at: -11 @ 70 font: nil color: Color brown.
   aCanvas drawString: 'IX' at: -90 @ -10 font: nil color: Color brown.


! !

!ReverseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:38'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
   inspectFlag := true.


! !

!ReverseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:38'!
stepAt: millisecondSinceLast
   "do some update"


! !

!ReverseButton methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:27:38'!
stepTime
  " milliseconds  "
  ^ 20 .

! !

!SpaceWar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:56:42'!
drawOn: aCanvas
   inspectFlag ifTrue:[ aCanvas inspect. 
	                            inspectFlag := false ].
"   aCanvas
      ellipseCenter: 0@0
      radius: 100
      borderWidth: 10
      borderColor: Color lightCyan
      fillColor: Color veryVeryLightGray.
"

   aCanvas drawString: 'XII' at: -13 @ -90 font: nil color: Color brown.
   aCanvas drawString: 'III' at: 66 @ -10 font: nil color: Color brown.
   aCanvas drawString: 'VI' at: -11 @ 70 font: nil color: Color brown.
   aCanvas drawString: 'IX' at: -90 @ -10 font: nil color: Color brown.


! !

!SpaceWar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:43:41'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
   inspectFlag := true.


! !

!SpaceWar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 18:21:09'!
stepAt: millisecondSinceLast
   "do some update"


! !

!SpaceWar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 18:19:30'!
stepTime
  " milliseconds  "
  ^ 20 .

! !

!StopButton2 methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:29:00'!
drawOn: aCanvas
   inspectFlag ifTrue:[ aCanvas inspect. 
	                            inspectFlag := false ].
"   aCanvas
      ellipseCenter: 0@0
      radius: 100
      borderWidth: 10
      borderColor: Color lightCyan
      fillColor: Color veryVeryLightGray.
"

   aCanvas drawString: 'XII' at: -13 @ -90 font: nil color: Color brown.
   aCanvas drawString: 'III' at: 66 @ -10 font: nil color: Color brown.
   aCanvas drawString: 'VI' at: -11 @ 70 font: nil color: Color brown.
   aCanvas drawString: 'IX' at: -90 @ -10 font: nil color: Color brown.


! !

!StopButton2 methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:29:00'!
initialize
  "do some setup of the space war program 
  black screen , yellow central star , red torpedo , colourful spaceships
  use arrow keys to control space ship
  "
   super initialize.
   inspectFlag := true.


! !

!StopButton2 methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:29:00'!
stepAt: millisecondSinceLast
   "do some update"


! !

!StopButton2 methodsFor: 'as yet unclassified' stamp: 'tc 8/30/2025 09:29:00'!
stepTime
  " milliseconds  "
  ^ 20 .

! !

!Torpedo methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 18:15:01'!
initialize
   super initialize.
   velocity := 0 @ 0.
   position := 100 @ 100.
   acceleration := 0! !
