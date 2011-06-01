concrete ConjunctionNep of Conjunction = 
  CatNep ** open ResNep, Coordination, Prelude in {


  flags optimize=all_subs ;

  lin

    ConjS  = conjunctDistrSS  ;

    ConjAdv = conjunctDistrSS ;
--    ConjAdv conj advs = conjunctDistrTable Gender conj advs ;

    ConjNP conj ss = conjunctDistrTable NPCase conj ss ** {
      a = conjAgr (agrP3 Masc conj.n) ss.a ;
--      isPron = ss.isPron ;
      } ;

    ConjAP conj ss = conjunctDistrTable2 Number Gender conj ss ; 
--    ConjRS conj rs = conjunctDistrTable Agr conj rs ** { c = rs.c};

---- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
--    BaseAdv = twoSS ;
    BaseAdv x y = twoSS x y  ;
    ConsAdv = consrSS comma ;
--    ConsAdv xs x = consrTable Gender comma xs x ;
    BaseNP x y = twoTable NPCase x y ** {a = conjAgr x.a y.a ; isPron = andB x.isPron y.isPron} ;
--    BaseRS x y = twoTable Agr x y ** {c = x.c};
    ConsNP xs x = consrTable NPCase comma xs x ** {a = conjAgr xs.a x.a } ;
--    ConsRS xs x = consrTable Agr comma xs x ** { c = xs.c};
    BaseAP x y = twoTable2 Number Gender x y ; -- ** {isPre = andB x.isPre y.isPre} ;
    ConsAP xs x = consrTable2 Number Gender comma xs x ;-- ** {isPre = andB xs.isPre x.isPre} ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : NPCase => Str ; a : Agr } ;
    [AP] = {s1,s2 : Number => Gender => Str} ;
    [RS] = {s1,s2 : Agr => Str ; c : Case};

}
