:- dynamic ([world_size/1,	
             breeze/1,
             wumpus_position/1,
             pit_position/1,
             gold_position/1,
             agent_position/1]).

%-------------------------initial configurations----------------------
wumpus_position(room(1,2)).
pit_position(room(1,2)).
pit_position(room(2,2)).
pit_position(room(3,4)).
gold_position(room(3,3)).
agent_position(room(1,1)).


%---------------------The boudaries of our world------------------------
accept(X) :- X is 1; X is 2; X is 3; X is 4.
bounds(X, Y) :- accept(X), accept(Y).
room(X, Y) :- bounds(X, Y).

%--------------------------Adjacent Rooms--------------------------------
adjacentTo(room(X, Y), room(A, B)) :- room(X, Y), room(A, B),
                                    (A is X-1, B is Y ;
                                    A is X+1, B is Y ;
                                    A is X, B is Y-1 ;
                                    A is X, B is Y+1).

%-------------------------- Finding Pit------------------------------------
pit(room(X,Y)) :- 
    %pit_position(room(X,Y)),
     forall(adjacentTo(room(X,Y),room(W,Z)), breeze(room(W,Z))),
    format("Pit in room (~w,~w) ~n", [X,Y]).

%-------------------------- Finding Breeze--------------------------------
breeze(room(X,Y)):-
    
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( pit_position(room(X1,Y)) ;
    pit_position(room(X0,Y)) ;
    pit_position(room(X,Y1)) ;
    pit_position(room(X,Y0))) ,
    !.

%-------------------------- Finding Stench--------------------------------
stench(room(X,Y)):-
     forall(wumpus_position(room(W,Z)), adjacentTo(room(X,Y),room(W,Z))).
    

%-------------------------- Finding Wumpus--------------------------------
wumpus(room(X,Y)):-
    forall(adjacentTo(room(X,Y),room(W,Z)), stench(room(W,Z))),
    format("Wumpus in room (~w,~w) ~n", [X,Y]).

%--------------------------Finding Safe Rooms--------------------------------
safe(room(W,Z)):-
    \+ wumpus_position(room(W,Z)), \+ pit_position(room(W,Z)),
    format("Room (~w,~w) is safe ~n", [W,Z]).

safe(room(X,Y), room(W,Z)):- %to get the safe positions from a given position
     
    adjacentTo(room(X,Y) , room(W,Z)) , \+ wumpus_position(room(W,Z)),  \+ pit_position(room(W,Z)). 
     
%-------------------------- Shooting The Wumpus--------------------------------
shootWumpus(room(X,Y)):-
    adjacentTo(room(X,Y),room(W,Z)) , wumpus_position(room(W,Z)),
    format("The shot has been done succesfully... ~n"),
    format("You Won... ~n").

%--------------------------Finding Gold--------------------------------
gold(room(X,Y)):-
    gold_position(room(X,Y)),
    format("There is gold in room (~w,~w) ~n", [X,Y]),
	format("Grab it to gain more scores...!").

%-------------------------- Grabing The Gold --------------------------------
grabGold(room(X,Y)):-
         agent_position(room(X,Y)), gold_position(room(X,Y)),
         format("You have grabed the gold in room (~w,~w) ~n", [X,Y]),
         format("Points have been added to your score...! ~n").


    
         
    
    
    
    
    
    
    
    
