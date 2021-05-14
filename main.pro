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








glanian(zhuirlu, q, [0.594, 0.827, 0.904, 0.962, 0.604, 0.218, 0.321, 0.733, 0.630, 0.057]).
glanian(josizar, g, [0.936, 0.425, 0.550, 0.613, 0.063, 0.334, 0.834, 0.242, 0.970, 0.172]).
glanian(olisor, q, [0.310, 0.043, 0.949, 0.722, 0.239, 0.909, 0.182, 0.635, 0.377, 0.639]).
glanian(calemi, g, [0.306, 0.507, 0.038, 0.075, 0.597, 0.479, 0.900, 0.289, 0.945, 0.331]).


expects(zhuirlu, [m, b, f], [0.945, -1, 0.186, 0.151, -1, -1, 0.483, 0.343, 0.351, 0.960]).
expects(josizar, [f, b, a], [0.213, 0.590, 0.599, -1, 0.040, -1, 0.678, 0.492, -1, 0.058]).
expects(olisor, [a], [-1, 0.657, -1, 0.990, 0.838, 0.295, -1, 0.075, 0.626, -1]).
expects(calemi, [q, f], [0.291, -1, 0.003, 0.351, -1, 0.516, 0.456, 0.019, 0.305, 0.445]).


weight(zhuirlu, [0.849, -1, 0.199, 0.280, -1, -1, 0.597, 0.125, 0.742, 0.242]).
weight(josizar, [0.332, 0.746, 0.203, -1, 0.115, -1, 0.106, 0.524, -1, 0.390]).
weight(olisor, [-1, 0.528, -1, 0.143, 0.272, 0.084, -1, 0.069, 0.125, -1]).
weight(calemi, [0.459, -1, 0.855, 0.258, -1, 0.288, 0.900, 0.093, 0.602, 0.510]).


likes(zhuirlu, [], [beyroot, istenbol]).
likes(josizar, [crafting, camping, swimming], [seviliri, viyan]).
likes(zhuirzaz, [camping, swimming, puppet_show, concert], [maimaician, san_wm, seviliri]).


city(venis, [zhuirlu, thinnest, shiryva, peregrey, celerot, zynthmys, ramgan, eligel, greei, bloshas, sudemi, emijosi, ausly, slycha, flammvae, peregnarv, faezab, logfros, drtist, eldvy, haesols, ponnal, sudjin, igninau, blosshee, chembell, dorsangu, mystbri, séveshir, torij, abigsapp, aurstar, aldig, shirlog, anthignin, midnbrilb, tureng, skelingi, cluang, darcaky, lurey, ilyrei, ylynzor, avel, daermort, hunnight, ophebra, yvapro, galste, lymshun, liduvr, zhuiron, elocira, livik, cryssnowf, josimire, nysyva, salves, pondfa, skelsop, bellsant, jaylfun, daeakym, rotnor, vayfin, auto, silverstar_, lipster, malblis, ganblis, vinefae, tistonn, aqualava, sudchem, ailséve, edmsir, zarsly, vaelof, fabtwil, vaybra, rainaus, dartist, sopgre, mistmist, foxfgay, oliusun, morthor, star_yrli, lisebas, kilel, syftaz, lunanor, evejayl, soldnal, calpond, ailrag, thrdar, haezhuir, edmdig, solshas, ciselo, aqrihad, missari, bliscira, ophefros, usno, vinezapp, stervik, diggol, solsnight, jodmal, tismal, drsols, usvir, ytanaid, usunlai, brilbeng, iogtwil, mortbon, erosab, elwmidn, thinbal, coagan, lulai, rijla, kalsale, dynaang, darcatist, eiliingi, ailtae, lucama, dezanys, tissilver, tallava, oraqri, brato, caebon, remingi, earthste, owthr, darfros, zorgal, gredrache, opheyrli, vixfab, funhun, wistzapp, renoddfi, briayld, evevay, ragkym, aulay, freyag, balgan, wittwil, twiledm, snowfmi, diamor, igningan, stersangu, malon, nightdiml, maloli, aidhae, josiama, zazvy, gozaz, brikus, haefros, nightnight, amataz, quinkys, talyag, elwyns, sirtae, alestar, rainsly, mortchem, twinmys, ranolw, daemster, reysor, sorpereg, aidzynth, jadechand, sabale, akkserpr, belldor, stechand, abigau, mortfun, kusfab, ezgal, angjade, santrem, aidvog, dagcis, kalsbel, bragel, jhapereg, daermine, amazhuir, vestal, dimlazu, turvfre, stepereg, sheejade, jewgus, sapmerl, fabast, shisangu, sorsyf, ilyrluna, sunstis, mystang, fabang, kalsero, anthglac, wistism, saetis, amerdyna, zhuirzak, pearlylyn, sudséve, soldeve, phydeza, klayem, santquam, ismalexa, scarero, brali, joddarca, gold_shad, lipwyns, slyzin, gaeemi, ertignin, gyllvae, emigolden, oliignin, ismla, bonesnanth, sévevae, olikals, yagcele, ophenal, norfre, dorfar, vesso, fresnowf, flammeld, dermeelo, brijayl, diamang, sorazr, malnight, yagazar, jodeve, mystgan, ilbshad, dracderme, cryscae, serprnarv, ismklay, alexafin, sozapp, belschand, oddfiyofune, jasdaer, zarkil, uldrgay, dermesab, ytandag, emragr, myrblis, phyrij, ponddaea, digelo, floig, kusdag, nalran, igusun, starduvr, aylddor, aurilyr, brilbkez, phytist, ragrren, eleccira, myrama, bonnbrilb, turveve, narvragr, kyanth, nelmalu, caelay, aqriny, kolzwil, erono, amacha, crysjayl, balmyst, bonesnsols, aylddiml, ninrain, snowfnel, blosblis, saeau, horyrli, shirjew, lucnys, froselo, olikus, azublis, azrpro, onfae, esmertwin, zhuirzapp, aumyst, anytan, haseste, gyllytan], [judo, pet, photo, crafting, card_game]).
city(corse_town, [josizar, draccor, evedaem, emjai-b, ylynpon, axelw, starnel, mirewilk, aurbel, prodar, quamlu, ameklay, sariazar, elecpha, alnest, akibzar, orkalo, jharain, nymidn, astdarca, nightpind, malyr, nestgemb, klaymaje, hunvir, twinow, zappstarb, eldophe, sabhep, shircira, lybre, zhuirpereg, gylltal, brilbjha, lymsaus, sybeserpr, ansyf, bonbonn, zarsapp, tanuazu, orifae, evesnowf, ranste, sirgold_, azaraus, thrbel, vixazu, mirem, dezasari, tendli, amerlog, ontiz, reyyag, avdrac, vogfae, vinetwin, malsor, ismvix, saeemi, frepon, shirdor, chemdark_, jingus, amerey, wynsdr, myraus, tizgel, hadelec, esteazu, nordr, ilbsant, wistemi, samaus, calbre, witmerl, vayytan, doreth, asrawist, bellhas, snowfblos, caemyst, earthyofune, sudduvr, tazluc, zazsul, shilavi, fabhas, pyrelo, haeluna, wispmsal, gyllon, nightsud, sakafoxf, yvaau, sebasaqri, norjas, talsud, zareth, anthsuns, bulcha, kalstha, mortdaer, sulquam, wistlyms, eldvine, zisaka, wisshi, brovae, sammaje, celetist, rainylyn, jadedeve, ragrayld, astlu, ilyrbrilb, azumort, solsmire, scarlai, lipsebas, reydag, ailsal, ziren, dracheky, glacquin, gold_gold_, sheecal, rortae, vaetist, gelvix, aqrivae, tovas, quinbra, zencha, zappsud, akkor, cosdaem, zappgyll, starbzab, zhuirnest, gomal, wilwilk, minesyf, myrtis, alexali, sybeny, yvanight, anaqua, jadeazu, fabglac, vinegae, noragr, igninwyns, haesop, vaemort, zaklay, pearlkus, ludarca, ethluc, alexaal, zynthkil, laili, phatal, asramal, myrmort, sévegay, jai-bsud, kysdrache, onayld, orifun, aquacae, abigbon, cislai, zigae, bonit, nestbel, gusshi, rhenaau, esmermys, darcaakib, onsols, branemre, libul, eiligolden, dracelw, solsclu, ayldwil, bricrys, nypro, zarrot, onnsols, kusclu, bonnsap, wilksapp, vasazr, silfre, bracoa, vaejew, sanguster, celenest, pindjod, pyreste, syfkys, lucytan, thanarv, ingisil, jhaso, lieste, azuig, azuzhuir, zabzar, nightnarv, dermesud, eldrus, tizsam, zingyll, vinesold, darcayr, lavaayld, chembre, bonfae, turhae, bulcele, nybels, balcae, alemerl, nalilb, avbel, dagasra, jhajosi, sameld, akkzar, tizbels, gocae, daryva, milkyhun, merlgolden, tendsols, sopblis, duvresmer, amerkalo, golno, ethero, lofpro, kalsgolden, dardor, angmal, amerfin, sybejai-b, braluc, zenyr, jaszor, golayld, foxfrij, kezzi, vikfar, sopei, soldau, daerfros, goldenlof, soldto, startend, blisluna, vineilb, thalu, star_night, nyzin, majeli, ganvine, sopem, lavivy, zynthzar, frecira, emanth, abigfun, quamhep, horazar, gallthin, astpearl, zhuirtis, vyfun, bonnpon, ertvine, azulu, zakang, brabonn, kysmidn, eleceth, pondignin, bridaea, goves, chasybe, noaur, clulu, sopjai-b, haedor, shirvae, alsab, iogwil, ezmal, kalsrain, drusklay, dynasir, mysail, nightbre, gyllophe, orsop, azugre, astgall, elwakib, oriderme, igningall, drachemal, jasfae, midname, itpro, digvae, akibilb, tendeili, sakaig, skelan, eldesmer, digdar, sophas, vesny, hadnys, kolzvae, dorglac, bliszi, kyhas, ylyntha, elsil, skeldor, rotingi, haekals, lucpyr, tazignin, dagedm, stargan, lueth, oristar_, asratha, amamalu, darcaal, brorot, brucele, laviazu, lymsilb, luvy, dignin, ayldves, yrlikys, drachevik, zorvay, dermeakib, bonesnoli, amamort], [walking, tennis, swimming, bird_watching, puppet_show, circus, board_gaming]).













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
	

find_possible_targets(Name, Distances, TargetList ) :-

	





























	
