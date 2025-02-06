
% output comes out on emulator
% C-. C-l oz-feed-line
{Show 'Hello World'}
{Show 'Hello World2'}
{Show 'Hello World3'}
% to 'see' the output oz-toggle-emulator C-. e
% to 'see' the output oz-toggle-compiler C-. c
% to feed a region oz-feed-region C-. C-r

% feed buffer oz-feed-buffer C-. C-b 
% open a new oz buffer oz-new-buffer C-. n

% 'Hello World'
% select hello world oz-inspect-region C-. i C-r

% 'Hello World'
% select hello world oz-browse-region C-. b C-r

% 123
% 123123123123123
% 100000000000000 + 342342342323
% 100000000000000 * 342342342323
% 12 % 4
% 12 mod 4
% 13 mod 3


% this gives some errors but we cant find out where it goes wrong .
local A B in 
   A = 3
   proc {B}
      {Show A + 'Tinman'}
   end 
   {B 7}
end

% can we do a factorial fun

% C-. C-r  oz-feed-region
% local definitions span between in ... end
local W H in
{Browse foo(width:W height:H surface:thread W*H end)}
W = 3
{Browse foo(width:W height:H surface:thread W*H end)}
H = 5 
{Browse foo(width:W height:H surface:thread W*H end)}
end

% declare - global definition of W H  reach forever
declare W H 
{Browse foo(width:W height:H surface:thread W*H end)}
W = 3
{Browse foo(width:W height:H surface:thread W*H end)}
H = 5 
{Browse foo(width:W height:H surface:thread W*H end)}


% oz-next-buffer M-n 
% oz-previous-buffer  M-p


% tree is a record 
declare T I Y LT RT W in 
T = tree(key:I value:Y left:LT right:RT)
I = seif
Y = 43
LT = nil
RT = nil
W = tree(I Y LT RT)
{Browse [T W]}

% Selecting a Component
{Browse T.key}
{Browse W.1}
% will show seif twice in the browser
seif
seif


% fixed closed records - there are open ended record variants but later ...
% as long as we have declared variable to be
% record rando with fields key value left right fourth
declare A B C D T Z in
T = rando(key:A value:B left:C right:D fourth:Z )
{Browse [T]}
A = 3  % an integer 
B = 4.14  % a float ?
D = &c %the character c
Z = "hello world"  % a string

% what libraries do we have available - can i do md5sum on a string and get the result , or sha512sum ?
% 

% do we have mixed types lists 
{Browse [1 3.4 "hello" &c ]}

"hello"  % [104 101 108 108 111] a list of ascii characters - so no unicode


% Getting the Arity of a Record
local X in {Arity T X} {Browse X} end 
local X in {Arity W X} {Browse X} end

% Selecting a component conditionally
local X in {CondSelect W key eeva X} {Browse X} end 
local X in {CondSelect T key eeva X} {Browse X} end

% cons operator 
1|2|3|nil
% same as [1 2 3]

1 | [4 5 6]  % gives [1 4 5 6 ] as expected


% base
local A in
   {String.toInt "-123232323223" A}
   {Browse A}
end

% how read a file contents - is this ascii too ?


% sort some values 
{List.Sort [c d b d a] Value.'<'}

{Sort [1 2 3 2 1 ] Value.'<'}

% make Xs a list of length 5 with unknowns
% is this helpful ?
local Xs in
   {List.make 5 Xs}
   {Browse Xs}
end

% way in which knowledge can become available after the fact 
local Xs Ys Tail in
   {List.make 10 Xs}
   {Browse Xs}
   Xs = 5 | 6 | Ys
   Ys = 10 | 11 | 12 | Tail
end

% is there not a combinatorial explosion then ...

% highlight region C-. C-r
{Browse 1 == 1 }
{Browse 1 == 2 }
{Browse {Not true }}
{Browse {Not false }}

{Browse {And true false}}
{Browse {And true true }}
{Browse {And false false}}
{Browse {And false true }}

% true and false are built-in ?
% And Or Not Xor
{Browse {Or true false}}

% how does this help us though ?


% if then end ...
local X Y Z in 
   X = 5 Y = 10
   if X >= Y then Z = X else Z = Y end
   {Browse [X Y Z]} % x=5 y=10 then z=10
end

% some sort of merge procedure sorting ...
proc {SMerge Xs Ys Zs}
   case Xs#Ys
   of nil#Ys then Zs=Ys
   [] Xs#nil then Zs=Xs
   [] (X|Xr) # (Y|Yr) then 
      if X=<Y then 
         Zs = X|{SMerge Xr Ys}
      else Zr in 
         Zs = Y|{SMerge Xs Yr}
      end 
   end 
end

% factorial
% fact 1 = 1 
local   proc {Fact2 Xs Ys}
	   local Xs2 Zr in
	      Xs2 = Xs - 1
	      {Fact2 Xs2 Zr}
	      Ys = Zr * Xs	 
	   end
	in
	   proc {Fact Xs Ys}
	      if Xs == 1 then Ys = 1
	      else
		 {Fact2 Xs Ys}
	      end
	   end
	end
end


% defined double function
declare Double
fun {Double Xs Ys}
   Ys = Xs + Xs
end
% make use of double to add 2 to 2 
local X in
   {Browse {Double 2 X}}
end


% % my fact 
% declare MyFact
% fun {MyFact Xs Ys}
%    if Xs == 1 then Ys = 1
%    else 
%       local Ys2 Ys3 in
% 	 Ys2 = Xs - 1 
% 	 % {MyFact Ys2 Ys3}
% 	 Ys = Xs * Ys2
% 	end
%    end   
% end
% % make use of factorial 5 
% local X in
%    {Browse {MyFact 5 X}}
% end



% my fact 
declare MyFact
fun {MyFact Xs Ys}
   if Xs == 1 then Ys = 1
   else
      local Ys2 in
      	 {MyFact (Xs - 1) Ys2}
	 Ys = Xs * Ys2
      end
   end   
end
% make use of factorial 5 
local X in
   {Browse {MyFact 5 X}}
end



% a factorial function 
declare
fun {Fact N}
if N==0 then 1 else N*{Fact N-1} end
end
% make use of factorial 5 
{Browse {Fact 100}}

declare
fun {Comb N K} /* computes the value of C(n, k) */
{Fact N} div ({Fact K}*{Fact N-K})
end
% make use of factorial 5 
{Browse {Comb 5 2}}


% we can do a recursive append 
declare
proc {Append2 L1 L2 L3}
case L1
of nil then L2=L3
[] X|M1 then L3=X|{Append2 M1 L2}
end
end
local Z in
{Append2 [1 2 3] [4 5 6] Z}
{Browse Z}
end


% not a tail recursive so builds up stack space ...to unwind
% now it works ...
declare
proc {Fact2 L1 L2}
   if L1 == 1 then L2 = 1
   else local L3 in
	   {Fact2 (L1 - 1) L3}
	   L2 = L3 * L1
	end
   end
end
local Z in
{Fact2 1000 Z}
{Browse Z}
end


% so can we do this backwards ? this doesnt work 
local Z in
{Fact2 Z 120}
{Browse Z}
end

% can we use factorial Fact function to do this ?
% looks like it reads okay, just returns a suspended computation 
local X Z in
   X = 120
   X = {Fact Z}
   {Browse Z}
end


% pair up
declare X PX
X = [3   4     4   3     2   5     1   3     3   9     3   3 ]

% can we destructure a list - yes
local L H T in
   L = [1 2 3]
   H | T = L
   {Browse H}
   {Browse T}
end

% cons lists into other lists
% ok 
{Browse [1 2 3] | [4 5 6]}

% is empty list same as nil ? 
local L in
   L = [0]
   {Browse L == nil}
end

% no such thing as empty list []
% consing to right associative 1 | 2 | 3 | L is 1 :: (2 :: (3 :: nil))
local L in
   L = nil
   {Browse 1 | 2 | 3 | L}
end


local H2 H2 T2 L1 T in
   L1 = [1 2 3 4 5]
   (H1 | T) = L1
   {Browse H1}
end

{Browse List.1 [1 2 3]}

{Browse List.hd [1 2 3]}


% destructure head tail okay
local R T in
   R | T = [1 2 3 4 5]
   {Browse R}
   {Browse T}
end

% can destructure multiple also 
local R R2 T in
   R | (R2 | T) = [1 2 3 4 5]
   {Browse R}
   {Browse R2}   
   {Browse T}
end



% pair up
declare
proc {Pairs L1 L2}
   local H1 H2 T L3
   in 
      if L1 == nil then L2 = nil
      else
	 (H1 | (H2 | T)) = L1
	 {Pairs T L3}
	 L2 = [H1 H2] | L3	 
      end
   end   
end

% if list give it does not have two values - an even number entries in list - just balks 
declare X PX
% X = [3   4     4   3     2   5     1   3     3   9     3   3 ]
X = [3   4     4   3     2   5     1   3     3   9     3  ]
{Pairs X PX}
local R in
   {Browse PX}
end


% we dont want to pair like this , we want to split list into two different lists 
declare
proc {Pairs L1 L2}
   if L1 == nil then L2 = nil
   else local H1 H2 T L3
	in 
	   (H1 | (H2 | T)) = L1
	   {Pairs T L3}
	   L2 = [H1 H2] | L3	 
	end
   end   
end
declare X PX
X = [3   4     4   3     2   5     1   3     3   9     3   3 ]
{Pairs X PX}
local R in
   {Browse PX}
end


declare
proc {Split L La Lb}
   if L == nil then
      La = nil
      Lb = nil
   else local Ha Hb T Ta Tb
	in 
	   (Ha | (Hb | T)) = L
	   {Split T Ta Tb}
	   La = Ha | Ta
	   Lb = Hb | Tb
	end
   end   
end
declare X PX PY
X = [3   4     4   3     2   5     1   3     3   9     3   3 ]
{Split X PX PY}
local R in
   {Browse 'first->' # PX}
   {Browse 'second->' # PY}
end

% can have skip like in python pass keyword

% got less than equal sign the wrong way around also 
{Browse 2 =< 3 }


% insertion sort - just values for now - fixed comparator
declare
proc {InSort L X La}
   local H T Lb
   in 	
      if L == nil
      then
	 La = X | nil
      else
	 (H | T) = L
	 if X =< H then
	    La = X | L
	    %{Show 'La => ' # La}
	    %La = true
	 else
	    {InSort T X Lb}
	    La = H | Lb
	    %La = false
	 end	   
      end
   end
end



declare X PX
X = nil
{InSort X 4 PX}
local R in
   {Browse 'before insert->' # X}
   {Browse 'after insert->' # PX}
end


declare X PX
X = [1 2 3]
{InSort X 4 PX}
local R in
   {Browse 'before insert->' # X}
   {Browse 'after insert->' # PX}
end


declare X PX
X = [1 2 3]
{InSort X 2 PX}
local R in
   {Browse 'before insert->' # X}
   {Browse 'after insert->' # PX}
end

declare X PX
X = [1 2 3]
{InSort X 1 PX}
local R in
   {Browse 'before insert->' # X}
   {Browse 'after insert->' # PX}
end


declare X PX
X = [1 2 3]
{InSort X 0 PX}
local R in
   {Browse 'before insert->' # X}
   {Browse 'after insert->' # PX}
end

% assuming its a sorted list then 

local La in
   {InSort nil 3 La}
   {Browse La}
end


declare
proc {MySort L R}
   local H T R2 T
   in 	
      if L == nil
      then
	 R = nil
      else
	 (H | T) = L
	 % sort tail of list
	 {MySort T R2}
	 % insert head into that sorted list
	 {InSort R2 H R}	 
      end
   end
end

local X in
   {MySort [ 5 4 3 2 1] X }
   {Browse X}
end

% split , then sort lists 
declare X % SA SB TA TB
X = [3   4     4   3     2   5     1   3     3   9     3   3 ]
{Split X PX PY}
{MySort PX TA}
{MySort PY TB}
% lets see 
{Browse 'first->' # PX}
{Browse 'second->' # PY}
{Browse 'sorted-first->' # TA}
{Browse 'sorted-second->' # TB}   


