'From Cuis7.5 [latest update: #7452] on 29 August 2025 at 7:56:44 pm'!
'Description '!
!provides: 'SpaceWar!!' 1 12!
SystemOrganization addCategory: #'SpaceWar!!'!


!classDefinition: #CentralStar category: #'SpaceWar!!'!
PlacedMorph subclass: #CentralStar
	instanceVariableNames: 'velocity position acceleration fColor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'CentralStar class' category: #'SpaceWar!!'!
CentralStar class
	instanceVariableNames: ''!

!classDefinition: #SpaceShip category: #'SpaceWar!!'!
PlacedMorph subclass: #SpaceShip
	instanceVariableNames: 'velocity position acceleration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'SpaceShip class' category: #'SpaceWar!!'!
SpaceShip class
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

!classDefinition: #Torpedo category: #'SpaceWar!!'!
PlacedMorph subclass: #Torpedo
	instanceVariableNames: 'velocity position acceleration'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'SpaceWar!!'!
!classDefinition: 'Torpedo class' category: #'SpaceWar!!'!
Torpedo class
	instanceVariableNames: ''!


!CentralStar commentStamp: '<historical>' prior: 0!
velocity position and acceleration!

!SpaceShip commentStamp: '<historical>' prior: 0!
velocitty position and acceleration!

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

!Torpedo commentStamp: '<historical>' prior: 0!
velocity position and acceleration!

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:08:20'!
drawOn: canvas
   | radius |
   radius := self morphExtent // 2.
   canvas ellipseCenter: 0 @ 0
      radius: (radius x + (2 atRandom - 1)) @ (radius y + (2 atRandom - 1))
      borderWidth: 3 
      borderColor: Color orange 
      fillColor: Color random.
      self changed.
! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:15:03'!
initialize
   super initialize.
   velocity := 0 @ 0.
   position := 100 @ 100.
   acceleration := 0.   
   fColor := Color blue .
! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 18:26:39'!
morphExtent
   ^ `100 @ 100`

! !

!CentralStar methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 19:18:35'!
step
  " do something "
  "self changed."
  "Transcript show: 'tick' ; cr ."
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

!SpaceShip methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 18:12:19'!
initialize
   super initialize.
   velocity := 0 @ 0.
   position := 100 @ 100.
   acceleration := 0! !

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

!Torpedo methodsFor: 'as yet unclassified' stamp: 'tc 8/29/2025 18:15:01'!
initialize
   super initialize.
   velocity := 0 @ 0.
   position := 100 @ 100.
   acceleration := 0! !
