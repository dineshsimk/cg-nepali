concrete NounNep of Noun = CatNep ** open ResNep, Prelude in {

  flags optimize = all_subs ;
  flags coding=utf8 ;

  lin
 
    -- DetCN   : Det -> CN -> NP ;   -- the man
    DetCN det cn = {
      s = \\c => detcn2NP det cn c det.n  ;
      a = toAgr det.n cn.h cn.g ;
      t = cn.t 
      } ;

    -- UsePN   : PN -> NP ;          -- John
    UsePN pn = {
		s = \\c => toNP pn.s c ; 
		a = toAgr Sg pn.h pn.g ; 
		t = pn.t 
		} ;
    
    -- UsePron : Pron -> NP ;        -- he
    UsePron p = {
		s = \\c => np2pronCase p.s c p.a ; 
		a = p.a ; 
		t = Living 
		} ;

	-- PredetNP : Predet -> NP -> NP; -- only the man 
    PredetNP pred np = {
      s = \\c => pred.s ++ np.s ! c ;
      a = np.a ;
      t = np.t 
      } ;

    -- Neds to change this, 
    -- needs to check for root ending case, now works for root2 cases only
    -- PredetNP : Predet -> NP -> NP; -- only the man 
    PPartNP np v2 = {
      s = \\c => case (fromAgr np.a).n of {
          Sg =>  case (fromAgr np.a).g of {
                  Masc => np.s ! c ++ v2.s ! Root ++ eko ;
                  Fem  => np.s ! c ++ v2.s ! Root ++ eki 
                } ; 
          Pl =>  np.s ! c ++ v2.s ! Root ++ eka
         } ;
      a = np.a ;
      t = np.t 
      } ;
    
    -- AdvNP   : NP -> Adv -> NP ;    -- Paris today
    AdvNP np adv = {
      s = \\c => np.s ! c ++ adv.s ; 
      a = np.a ;
      t = np.t
      } ;
    
    -- RelNP   : NP -> RS  -> NP ;    -- Paris, which is here
    RelNP np rs = {
	  s = \\c => np.s ! c  ++ "," ++ rs.s ! np.a ;
      a = np.a ;
      t = np.t 
      } ;
    
	-- DetNP   : Det -> NP ;  -- these five
    DetNP det = {
      s = \\c => det2NP det c ; 
      a = agrP3 Masc Sg ;
      t = NonLiving 
      } ;
    
    -- DetQuant    : Quant -> Num ->        Det ;  -- these five
    DetQuant quant num = {
      s = \\n,g => quant.s ! num.n ! g ++ num.s;
	  n = num.n
      } ;
    
    -- DetQuantOrd : Quant -> Num -> Ord -> Det ;  -- these five best
    DetQuantOrd quant num ord = {
      s = \\n,g => quant.s ! n ! g ++ ord.s ++ num.s ; 
      n = num.n
      } ;
    
      
    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;
    NumCard n = n ** {hasCard = True} ;
    
	-- NumDigits  : Digits  -> Card ;  -- 51
	NumDigits n = {s = n.s ! NCard ; n = n.n} ;
	
	-- NumNumeral : Numeral -> Card ;  -- fifty-one
	NumNumeral numeral = {s = numeral.s ! NCard; n = numeral.n} ;
	
	-- AdNum : AdN -> Card -> Card ;   -- almost 51
	AdNum adn num = {s = adn.s ++ num.s ; n = num.n} ;
	
	
    OrdDigits n = {s = n.s ! NOrd  ; n = n.n} ;
    OrdNumeral numeral = {s = numeral.s ! NOrd ; n = numeral.n} ;
    OrdSuperl a = {s = sbvn ++ a.s ! Sg ! Masc  ; n = Sg} ;
	
	   
	IndefArt = {s = \\_,_ => [] } ;
    DefArt = {s = \\_,_ => [] } ;
    
    MassNP cn = {
        s = \\c => toNP (cn.s ! Sg) c ; 
        a = toAgr Sg cn.h cn.g ; 
        t = cn.t 
        } ; 

    DetArtSg art cn = {
      s = \\c => art.s ++ toNP (cn.s ! Sg) c ;
      a = Ag cn.g Sg cn.h
      } ;
   
   
    DetArtPl art cn = {
      s = \\c => art.s ++ toNP (cn.s ! Pl) c ;
      a = toAgr Pl cn.p cn.g
      } ;
   
      
    -- PossPron : Pron -> Quant ;    -- my (house)
    PossPron p = {s = \\_,_ => p.ps } ;
    
    
--2 Common nouns

    UseN n = n ;
    
    -- ComplN2 : N2 -> NP -> CN ;    -- mother of the king
    ComplN2 f np = {
       s = \\n,c => np.s ! NPC Nom ++ f.c2 ++ f.s ! n ! c ;
	   g = f.g ;
       t = np.t ;
       --h = x.h 
       h = (fromAgr np.a).p
	   } ;

    -- ComplN3 : N3 -> NP -> N2 ;    -- distance from this city (to Paris)
    ComplN3 f x = {
      s = \\n,c =>  x.s ! NPObj ++ f.c3 ++ f.c4 ++ f.s ! n ! Nom  ;
      g = f.g ;
      t = f.t ;
      h = f.h ;
      c2 = f.c2 ;
      c3 = f.c3
      } ;
      
    
    -- UseN2   : N2 -> CN ;          -- mother
    UseN2 n2 = { s = n2.s  ; g = n2.g ; t = n2.t ; h = n2.h };

    -- Use2N3  : N3 -> N2 ;          -- distance (from this city)
    Use2N3 f = {
      s = f.s ;
      g = f.g ;
      t = f.t ;
      h = f.h ;
      c2 = f.c2 ;
      c3 = f.c3
      } ;

    -- Use3N3  : N3 -> N2 ;          -- distance (to Paris)
    Use3N3 f = {
      s = f.s ;
      g = f.g ;
      t = f.t ;
      h = f.h ;
      c2 = f.c2 ;
      c3 = f.c3
      } ;

    
    -- AdjCN   : AP -> CN  -> CN ;   -- big house
    AdjCN ap cn = {
      s = \\n,c => ap.s ! n ! cn.g  ++ cn.s ! n ! c ;
      g = cn.g ; 
      t = cn.t ;
      h = cn.h
      } ;

    -- RelCN   : CN -> RS  -> CN ;   -- house that John bought
    RelCN cn rs = {
      s = \\n,c => cn.s ! n ! c ++ rs.s ! agrP3 cn.g n ;
      g = cn.g ;
      t = cn.t ;
      h = cn.h
      } ;
    
    -- AdvCN   : CN -> Adv -> CN ;   -- house on the hill
    AdvCN cn ad = {
		s = \\n,c => cn.s ! n ! c ++ ad.s  ; 
		g = cn.g ; 
		t = cn.t ; 
		h = cn.h
		} ;
    
    --Prelude.glue 
    -- SentCN  : CN -> SC  -> CN ;   -- question where she sleeps
    SentCN cn sc = {s = \\n,c => sc.s ++ "लाइ" ++ cn.s ! n ! c ; g = cn.g ; t = cn.t ; h = cn.h} ;
    -- Changed to fix 'reason to sleep' Bug
    -- SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g ; t = cn.t ; h = cn.h} ;
    
--2 Apposition

    -- ApposCN : CN -> NP -> CN ;    -- city Paris (, numbers x and y)
    ApposCN cn np = {s = \\n,c => cn.s ! n ! Nom ++ np.s ! NPC c ; g = cn.g ; t = cn.t ; h = cn.h} ;

}
