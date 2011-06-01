concrete IdiomPnb of Idiom = CatPnb ** open Prelude,Predef, ResPnb in {

  flags optimize=all_subs ;
  flags coding = utf8;

  lin
    ImpersCl vp = mkSClause " " (agrP3 Masc Sg) vp ;
    GenericCl vp = mkSClause "kwy" (agrP3 Masc Sg) vp ;

    CleftNP np rs = 
	 let cl = mkSClause (np.s ! NPC rs.c) (np.a) (predAux auxBe);
	  in 
	   {s = \\t,p,o =>  cl.s ! t ! p ! o ++ rs.s ! np.a };
	  
    CleftAdv ad ss = { s = \\t,b,o => ad.s ! Masc ++ ss.s};
        
    ExistNP np = 
      mkSClause "awth'E" (agrP3 (fromAgr np.a).g (fromAgr np.a).n) 
        (insertObj (\\_ => np.s ! NPC Obl) (predAux auxBe)) ;

    ExistIP ip = 
     let cl = mkSClause ("awth'E" ++ ip.s ! Dir) (agrP3 ip.g ip.n) (predAux auxBe); 
	   in {
       s = \\t,p,qf => case qf of { 
	      QDir =>   cl.s ! t ! p ! ODir;
          QIndir => cl.s ! t! p ! ODir
		  }
		};

--    ProgrVP vp = insertObj (\\a => vp.obj.s ++ vp.ad ++ vp.comp ! a ++ (vp.s ! VPStem).inf ++ raha (fromAgr a).g (fromAgr a).n ) (predAux auxBe) ;
    ProgrVP vp =  (predProg vp) ;


    ImpPl1 vp = {s = "Aw" ++ infVP True vp (agrP1 Masc Pl)} ;
	ImpP3 np vp = {s = np.s!NPC Dir ++ "nwN" ++ (vp.s ! VPImp ).inf ++ "dyw"};


}

