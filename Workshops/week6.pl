:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).

/*
Q1 - borders(australia, X).
Q2 - borders(france, X), borders(spain, X). 
*/

country(C) :- country(C,_,_,_,_,_,_,_).

larger(Country1, Country2) :-
        area(Country1, Area1),
        area(Country2, Area2),
        Area1 > Area2.

area(Country, Area) :- country(Country,_,_,_,Area,_,_,_).

river_country(River, Country) :-
        river(River, Countries),
        member(Country, Countries),
        country(Country).      

country_region(Country, Region) :-
        region(Country, Region).
                
region(Country, Region) :- country(Country,Region,_,_,_,_,_,_).