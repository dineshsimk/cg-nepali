concrete NounNep of Noun = CatNep ** open ResNep, Prelude in {

  flags optimize=all_subs ;

  lin
 
    DetCN det cn = {
      s = \\c => detcn2NP det cn c det.n  ;
      a = agrP3 cn.g det.n ;
      } ;

    UsePN pn = {s = \\c => toNP pn.s c ; a = agrP3 pn.g Sg } ;
    UsePron p = {s = \\c => np2pronCase p.s c p.a ; a = p.a } ; --- ??? a not taken

    PredetNP pred np = {
      s = \\c => pred.s ++ np.s ! c ;
      a = np.a
      } ;

    PPartNP np v2 = {
      -- VF NPresent Imperf Pos (fromAgr np.a).p (fromAgr np.a).n (fromAgr np.a).g
      s = \\c => case (fromAgr np.a).n of {
         Sg =>  np.s ! c ++ v2.s ! Root ++ eko;
         Pl =>  np.s ! c ++ v2.s ! Root ++ eka
         } ;
      a = np.a
      } ;
    
    RelNP np rs = {
	  s = \\c => np.s ! c  ++ rs.s ! np.a ;
      a = np.a 
      } ;

    AdvNP np adv = {
      s = \\c => np.s ! c ++ adv.s ; -- from WHY??
      a = np.a 
      } ;

    DetNP det = {
      s = \\c => det2NP det c ; ---- case
      a = agrP3 Masc Sg
      -- isPron = False
      } ;
    
    -- ?? quant
    DetQuantOrd quant num ord = {
--      s = \\ c => detquant2det quant.s num.s c  ++ ord.s ; 
      s = \\n,g => quant.s ! n ! g ! Nom ++ ord.s ++ num.s ; 
      n = num.n
      } ;

    DetQuant quant num = {
--      s = \\c => detquant2det quant.s num.s c; 
      s = \\n,g => quant.s!n!g!Nom ++ num.s;
	  n = num.n
      } ;
      
    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;

    NumCard n = n ** {hasCard = True} ;


    -- PossPron p = {s = \\_,_,_ => p.ps ; a = p.a} ;

    
    NumDigits n = {s = n.s ! NCard ; n = n.n} ;
    OrdDigits n = {s = n.s ! NOrd; n = n.n} ;

    NumNumeral numeral = {s = numeral.s ! NCard; n = numeral.n} ;
    OrdNumeral numeral = {s = numeral.s ! NOrd ; n = numeral.n} ;

    AdNum adn num = {s = adn.s ++ num.s ; n = num.n} ;

    OrdSuperl a = {s = a.s ! Sg ! Masc  ; n = Sg} ;

    DetArtSg art cn = {
      s = \\c => art.s ++ toNP (cn.s ! Sg) c ;
      a = agrP3 cn.g Sg
      } ;

    DetArtPl art cn = {
      s = \\c => art.s ++ toNP (cn.s ! Pl) c ;
      a = agrP3 cn.g Pl
      } ;

    DefArt = {s = \\_,_,_ => [] ; a = defaultAgr} ;
    IndefArt = {s = \\_,_,_ => [] ; a = defaultAgr } ;

    MassNP cn = {s = \\c => toNP (cn.s ! Sg) c ; a = agrP3 cn.g Sg } ;

    UseN n = n ;
    UseN2 n = { s = n.s  ; g = n.g };

    Use2N3 f = {
      s = f.s;
      g = f.g ;
      c2 = f.c2;
      c3 = f.c3
      } ;

    Use3N3 f = {
      s = f.s ;
      g = f.g ;
      c2 = f.c2;
      c3 = f.c3
      } ;

    ComplN2 f x = {s = \\n,c => x.s ! NPC Nom ++ f.c2  ++ f.s ! n ! c ;
--	   Obl => x.s ! NPC c ++ f.c3 ++ f.s ! n ! c ;
--	   Abl => x.s ! NPC c ++ f.c3 ++ f.s ! n ! c ;
--	   ResPnb.Voc => x.s ! NPC c ++ f.c3 ++ f.s ! n ! c 
--	   };
	   g = f.g;
	   };

    ComplN3 f x = {
      s = \\n,c =>  x.s ! NPObj ++ f.c4  ++ f.s ! n ! Nom  ;
      g = f.g ;
      c2 = f.c2;
      c3 = f.c3
      } ;

    AdjCN ap cn = {
      s = \\n,c => ap.s ! n ! cn.g  ++ cn.s ! n ! c ;
      g = cn.g
      } ;

    RelCN cn rs = {
      s = \\n,c => cn.s ! n ! c ++ rs.s ! agrP3 cn.g n ;
      g = cn.g
      } ;
    
    AdvCN cn ad = {s = \\n,c => cn.s ! n ! c ++ ad.s  ; g = cn.g} ;

    SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g} ;

    ApposCN cn np = {s = \\n,c => cn.s ! n ! Nom ++ np.s ! NPC c ; g = cn.g} ;

}
