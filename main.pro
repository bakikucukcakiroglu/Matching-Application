% include the knowledge base
:- ['load.pro'].

% 3.1 glanian_distance(Name1, Name2, Distance) 5 points

% 3.2 weighted_glanian_distance(Name1, Name2, Distance) 10 points

% 3.3 find_possible_cities(Name, CityList) 5 points

% 3.4 merge_possible_cities(Name1, Name2, MergedCities) 5 points

% 3.5 find_mutual_activities(Name1, Name2, MutualActivities) 5 points

% 3.6 find_possible_targets(Name, Distances, TargetList) 10 points

% 3.7 find_weighted_targets(Name, Distances, TargetList) 15 points

% 3.8 find_my_best_target(Name, Distances, Activities, Cities, Targets) 20 points

% 3.9 find_my_best_match(Name, Distances, Activities, Cities, Targets) 25 points

 human(john).
 human(jack).
 human(alice).
 human(bob).

likes(john, alice).
likes(bob, alice).
likes(alice, bob).

similar_taste(Name1, Name2) :- likes(Name1, Something), likes(Name2, Something), \+(Name1=Name2).

length_of([],0).
length_of(List, Length):- 

	[Head|Tail]= List, 
	length_of(Tail, TailLength),
	Length is TailLength+1.
multiply_two([],[]).

multiply_two([Head|Tail], Result) :-
		
	multiply_two(Tail, TailResult), 
	TempHead is Head*2,
	Result = [TempHead| TailResult].

















concatenate([],List,List).
concatenate([H1|T1], List2, Result) :-

	concatenate(T1, List2, TailResult),
	Result = [H1| TailResult].





weighted_sum([],[],[], 0).
weighted_sum(ExpectedFeatures, GlanianFeatures, WeightList, Result) :-

	( [-1|X] = ExpectedFeatures
	->  [FirstEx|TailEx] = ExpectedFeatures,
		[FirstGla|TailGla]=GlanianFeatures,
		[FirstWeight|TailWeight]= WeightList,
		Temp is 0,
		weighted_sum(TailEx, TailGla, TailWeight, Result2),
		Result is Temp+ Result2
	;   [FirstEx|TailEx] = ExpectedFeatures,
		[FirstGla|TailGla]=GlanianFeatures,
		[FirstWeight|TailWeight]= WeightList,
		Temp is FirstWeight*(FirstEx-FirstGla)*(FirstEx-FirstGla),
		weighted_sum(TailEx, TailGla, TailWeight, Result2),
		Result is Temp+ Result2   
	).




sum([],[], 0).
sum(ExpectedFeatures, GlanianFeatures, Result) :-
	
	( [-1|X]=ExpectedFeatures
	-> [FirstEx|TailEx]=ExpectedFeatures,
	   [FirstGla|TailGla]=GlanianFeatures,
	   Temp is 0,
	   sum(TailEx, TailGla, Result2),
	   Result is Temp+Result2
	;  [FirstEx|TailEx]=ExpectedFeatures,
	   [FirstGla|TailGla]=GlanianFeatures,
	   Temp is (FirstEx-FirstGla)*(FirstEx-FirstGla),
	   sum(TailEx, TailGla, Result2),
	   Result is Temp+Result2
	).
	



glanian_distance(Name1, Name2, Distance) :-
	
	expects(Name1, ExpectedGenders, ExpectedFeatures),
	glanian(Name2,GlanianGender, GlanianFeatures),
	sum(ExpectedFeatures,GlanianFeatures,Result),!,
	Distance is sqrt(Result).




weighted_glanian_distance(Name1, Name2, Distance) :-

	expects(Name1, ExpectedGenders, ExpectedFeatures),
	glanian(Name2, GlanianGender, GlanianFeatures),
	weight(Name1, WeightList),
	weighted_sum(ExpectedFeatures, GlanianFeatures, WeightList, Result),!,
	Distance is sqrt(Result).




find_possible_cities(Name, CityList) :-
	
	city(CityName, HabitantList, ActivityList),  
	    (member(Name, HabitantList)
	->  findall(CityName, member(Name, HabitantList) , CurrentCity)
	),
	likes(Name, LikedActivities, LikedCities),
	append( CurrentCity, LikedCities, CityList).




merge_possible_cities(Name1, Name2, CityList) :-
	
	    (\+(Name1 = Name2)
	->  find_possible_cities(Name1, CityList1),
	    find_possible_cities(Name2, CityList2),!,
	    append(CityList1, CityList2, CityList)
    ;   find_possible_cities(Name1, CityList),!
    ).




find_mutual_activities(Name1, Name2, ActivityList) :-
	
	likes(Name1, LikedActivities1, LikedCities1),
	likes(Name2, LikedActivities2, LikedCities2),
	findall(Activity, (member(Activity, LikedActivities1), member(Activity, LikedActivities2)), ActivityList).
	



list_converter([], [],[]).
list_converter([H|T], Distances, TargetList) :-
	
	list_converter(T, TDistances, TTargetList),
	[First|Second]=H,
	Distances=[Second|TDistances],
	TargetList=[First| TTargetList].




find_possible_targets(Name, Distances, TargetList ) :-
	
	expects(Name, ExpectedGenders, ExpectedFeatures),!,
	findall([Name2|Distance2],(glanian(Name2, GlanianGender, GlanianFeatures),glanian_distance(Name,Name2,Distance2), member(GlanianGender, ExpectedGenders)), TempList),
	sort(2,  @=<, TempList, SortedTempList),
	list_converter(SortedTempList, Distances, TargetList).




find_weighted_targets(Name, Distances, TargetList) :- 
	
	expects(Name, ExpectedGenders, ExpectedFeatures),!,
	findall([Name2|Distance2],(glanian(Name2, GlanianGender, GlanianFeatures),weighted_glanian_distance(Name,Name2,Distance2), member(GlanianGender, ExpectedGenders)), TempList),
	sort(2,  @=<, TempList, SortedTempList),
	list_converter(SortedTempList, Distances, TargetList).




bool_are_liked_activities_in_target_city([FirstLiked|TailLiked], CurrentCity, Name2) :-
	
	city(CurrentCity, HabitantList, ActivityList),
	dislikes(Name2, DislikedActivities, DislikedCities, Limits),!,
	bool_are_liked_activities_in_target_city(TailLiked, CurrentCity,Name2),
	member(FirstLiked, ActivityList),
	\+ member(FirstLiked, DislikedActivities).
	



intersection_counter([],NameTarget, 0).
intersection_counter([FirstDisliked|TailDisliked], LikedActivities, Count) :-

		intersection_counter(TailDisliked, LikedActivities, CountTail),
			(member(FirstDisliked, LikedActivities)
		-> 	Count is CountTail+1
		;	Count is CountTail
		).	




bool_in_tolerance_limits( [], [],1).
bool_in_tolerance_limits( [FirstList|TailList], [FirstFeature|TailFeature],R) :-

		(FirstList=[]
	->  bool_in_tolerance_limits( TailList, TailFeature,RT),
		R is 1
	;   [FirstFirst| SecondFirstt] = FirstList,
		[SecondFirst|T]= SecondFirstt,
		((FirstFeature<SecondFirst), (FirstFeature>FirstFirst)),
		bool_in_tolerance_limits( TailList, TailFeature,RT),
		R is 1
	).




test(Name, Name2, Count) :-
	
	dislikes(Name2, DislikedActivities, DislikedCities, Limits),
	likes(Name, LikedActivities, LikedCities),				

	intersection_counter(DislikedActivities, LikedActivities, Count).


	
	
find_my_best_target(Name, Distances, ActivityList, CityList, TargetList) :-

	findall( [Distance,Activity,CityName,Name2], 
	(glanian(Name2, GlanianGender, GlanianFeatures),
	city(CityName, HabitantList, ActivityList),
	expects(Name, ExpectedGenders, ExpectedFeatures),
	likes(Name, LikedActivities, LikedCities),				
	dislikes(Name, DislikedActivities, DislikedCities, Limits),	

	\+ old_relation([Name, Name2]),
	member(GlanianGender, ExpectedGenders),	

	bool_in_tolerance_limits( Limits, GlanianFeatures ,R),
	intersection_counter( DislikedActivities, LikedActivities, Count), Count<3,

	\+ member(CityName, DislikedCities),
	merge_possible_cities(Name, Name2, MergedCities),
	member(CityName, MergedCities),
	member(Activity, ActivityList),
	\+ member(Activity, DislikedActivities),

	((find_possible_cities(Name, PossibleCities), member(CityName, PossibleCities) ); member(Activity, LikedActivities)),

	weighted_glanian_distance(Name, Name2, Distance)
	),
	TempList),
	sort(TempList, TargetList).




find_my_best_match(Name, Distances, ActivityList, CityList, TargetList) :-

	findall( [Distance,Activity,CityName,Name2], 
	(glanian(Name, GlanianGender, GlanianFeatures),
	glanian(Name2, GlanianGender2, GlanianFeatures2),
	city(CityName, HabitantList, ActivityList),
	expects(Name, ExpectedGenders, ExpectedFeatures),
	expects(Name2, ExpectedGenders2, ExpectedFeatures2),
	likes(Name, LikedActivities, LikedCities),
	likes(Name2, LikedActivities2, LikedCities2),							
	dislikes(Name, DislikedActivities, DislikedCities, Limits),	
	dislikes(Name2, DislikedActivities2, DislikedCities2, Limits2),	

	\+ old_relation([Name, Name2]),
	member(GlanianGender, ExpectedGenders2),member(GlanianGender2, ExpectedGenders),

	bool_in_tolerance_limits( Limits, GlanianFeatures2,R), bool_in_tolerance_limits( Limits2, GlanianFeatures,R2),
	(intersection_counter( DislikedActivities, LikedActivities2, Count), Count<3), (intersection_counter( DislikedActivities2, LikedActivities, Count2), Count2<3), 

	\+ member(CityName, DislikedCities), \+ member(CityName, DislikedCities2),

	merge_possible_cities(Name, Name2, MergedCities),
	member(CityName, MergedCities),
	member(Activity, ActivityList),
	\+ member(Activity, DislikedActivities), \+ member( Activity, DislikedActivities2),

	((find_possible_cities(Name, PossibleCities), member(CityName, PossibleCities) ); member(Activity, LikedActivities)),
	((find_possible_cities(Name2, PossibleCities2), member(CityName, PossibleCities2) ); member(Activity, LikedActivities2)),


	weighted_glanian_distance(Name, Name2, Distance1), weighted_glanian_distance(Name2, Name, Distance2), 
	Distance is (Distance1+Distance2)/2
	),
	TempList),
	sort(TempList, TargetList).



	





















	
