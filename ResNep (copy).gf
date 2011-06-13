--# -path=.:../abstract:../common:../../prelude
--
--1 Pnbu auxiliary operations.
--
-- This module contains operations that are needed to make the
-- resource syntax work. 

resource ResNep = ParamX  ** open Prelude,Predef in {

  flags optimize=all ;
  coding = utf8;

  param 
    Case = Nom | Acc | Ins | Dat | Abl | Loc ;
    Gender = Masc | Fem ;
	VTense = NPresent | NPast PTypes | NFuture FTypes;
    PTypes = Simpl | Hab | Unknown ;
    FTypes  = Defin | NDefin ;
    NPerson = Pers1 
	    | Pers2_L
	    | Pers2_M
	    | Pers2_H
	    | Pers3_L
        | Pers3_M
        | Pers3_H ;
		
	Order = ODir | OQuest ;

{-	
--2 For $Relative$
 
    RAgr = RNoAg | RAg Agr ;
    RCase = RC Number Case ;
-}

-- for Numerial
   
   CardOrd = NCard | NOrd ;
{-  
  -----------------------------------------
  -- Punjabi Pronouns
  -----------------------------------------
  
   Pronoun = P Number Gender Case NPerson;
   PersPronForm = PPF Number NPerson Case;
   
-------------------------------------------
--Verbs
-------------------------------------------
-}
      
    VerbForm =
      VF VTense Aspect Polarity NPerson Number Gender
      | Root;
      
  -- Aspect Perfective and non-perfective
    Aspect = Perf | Imperf ; 
  
  oper
    -- Noun = {s : Number => Case => Str ; g : Gender ; isHum : Bool } ;
    Noun = {s : Number => Case => Str ; g : Gender } ;
 
    Verb = {s : VerbForm => Str} ;

    Preposition = {s : Str};

--    DemPronForm = {s : Number => Gender => Case => Str};
--    PossPronForm = {s : Number => Gender => Case => Str};
    Determiner = {s : Number => Gender => Str ; n : Number};
{-  
-- a useful oper
    eq : Str -> Str -> Bool = \s1,s2-> (pbool2bool (eqStr s1 s2)) ;
-} 
-----------------------------------------------
-- Nepali Adjectives
-----------------------------------------------
   npAdjective = {s : Number => Gender => Str} ;

   mkAdjnp : Str -> npAdjective = \str ->
     case str of {
        st + "ो" => mkAdj1 str (st+"ी") (st+"ा") ;
        _        => mkAdj1 str str str
     } ;
   
   mkAdj1 : (x1,_,x3 : Str) -> npAdjective = 
     \sm, sf, smf -> {
        s = table {
            Sg => table {
                Masc => sm ;
                Fem  => sf 
            } ;
            Pl => table {
                Masc => smf ;
                Fem  => smf 
            } 
        }
     };

  --RefPron : Str;
  --RefPron = "खौद";

  
  ----------------------------------------------------------
  -- Grammar part
  ----------------------------------------------------------
  
  param
    Agr = Ag Gender Number NPerson ;
    NPCase = NPC Case | NPObj | NPErg ;

  oper

    {-
    np2pronCase :  (Case => Str) -> NPCase -> Agr -> Str = 
      \ppf,npc,a -> case npc of {
        NPC c => ppf ! c ;
        NPObj => ppf ! Nom ;
        NPErg => case (fromAgr a).p of {
           (Pers3_Near|Pers3_Distant) => ppf ! Nom ++ "ले" ;
	   _			     => ppf ! Nom
       }        
      } ;
    -}
    
	toNP : (Case => Str) -> NPCase -> Str = \pn, npc -> case npc of {
      NPC c => pn ! c ;
      NPObj => pn !  Nom ;
      NPErg => pn !  Nom ++ "ले"
      } ;
	
    -- ???? Hardcorded gender and Number
    -- this/these
    detcn2NP : (Determiner) -> Noun -> NPCase -> Number -> Str = \dt,cn,npc,nn -> case npc of {
       NPC c => dt.s ! dt.n ! Masc ++ cn.s ! nn ! c ;
       NPObj => dt.s ! dt.n ! Masc ++ cn.s ! nn ! Nom ;
       NPErg => dt.s ! dt.n ! Masc ++ cn.s ! nn ! Nom ++ "ले"
      } ;  
    
    det2NP : (Determiner) -> NPCase -> Str = \dt,npc -> case npc of {
       NPC c => dt.s ! dt.n ! Masc ;
       NPObj => dt.s ! dt.n ! Masc ;
       NPErg => dt.s ! dt.n  ! Masc ++ "ले"
      } ;    

------------------------------------
-- Agreement transformations
------------------------------------

  oper

    toAgr : Number -> NPerson -> Gender -> Agr = \n,p,g ->       
	   Ag g n p;
      
    fromAgr : Agr -> {n : Number ; p : NPerson ; g : Gender} = \a -> case a of {
      Ag g n p => {n = n ; p = p ; g = g} 
	  } ;
	
    {-  
	conjAgr : Agr -> Agr -> Agr = \a0,b0 -> 
      let a = fromAgr a0 ; b = fromAgr b0 
      in
      toAgr
        (conjNumber a.n b.n)
        b.p a.g;	  
	
	giveNumber : Agr -> Number =\a -> case a of {
	   Ag _ n _ => n
	};
	giveGender : Agr -> Gender =\a -> case a of {
	   Ag g _ _ => g
	};
    -}    
    defaultAgr : Agr = agrP3 Masc Sg ;
    agrP3 : Gender -> Number -> Agr = \g,n -> Ag g n Pers3_M ;	
    personalAgr : Agr = agrP1 Masc Sg ;
    agrP1 : Gender -> Number -> Agr = \g,n -> Ag g n Pers1 ;
{-	
 param
      CPolarity = 
       CPos
       | CNeg Bool ;  -- contracted or not

 oper
    contrNeg : Bool -> Polarity -> CPolarity = \b,p -> case p of {
    Pos => CPos ;
    Neg => CNeg b
    } ;

-}
	NP : Type = {s : NPCase => Str ; a : Agr} ;

{-   
 param
    CTense = CPresent | CPast | CFuture ;
    
  oper 
    copula : CTense -> Number -> NPerson -> Gender -> Str = \t,n,p,g -> 
      case <t,n,p,g> of {
        <CPresent,Sg,Pers1,_   >        => "ौाण" ;
        <CPresent,Sg,Pers2_Casual,_   > => "ायण" ;
	    <CPresent,Sg,Pers2_Respect,_  > => "ाौ" ;
        <CPresent,Sg,_,_   > => "ाै" ;
        <CPresent,Sg,Pers3_Distant,_   > => "ाै" ;
	    <CPresent,Pl,Pers1,_   > => "ौाण" ;
        <CPresent,Pl,Pers2_Casual,_   > => "ाौ" ;
	    <CPresent,Pl,Pers2_Respect,_   > => "ाौ" ;
        <CPresent,Pl,_,_   > => "नै" ;
        <CPresent,Pl,Pers3_Distant,_   > => "नै" ;
        
	    <CPast,Sg,Pers1,_   > => "साण" ;
        <CPast,Sg,Pers2_Casual,Masc   > => "सय" ;
	    <CPast,Sg,Pers2_Casual,_   > => "सयण" ;
	    <CPast,Sg,Pers2_Respect,_   > => "सौ" ;
        <CPast,Sg,Pers2_Respect,Fem   > => "सयण" ;
        <CPast,Sg,_,_   > => "सय" ;
        <CPast,Sg,Pers3_Near,Fem   > => "तh-य" ;
        <CPast,Sg,Pers3_Distant,_  > => "सय" ;
        <CPast,Sg,Pers3_Distant,Fem  > => "तh-य" ;
        <CPast,Pl,Pers1,_   > => "साण" ;
        <CPast,Pl,Pers2_Casual,_   > => "सौ" ;
        <CPast,Pl,Pers2_Respect,_   > => "सौ" ;
        <CPast,Pl,_,_   > => "सण" ;
        <CPast,Pl,Pers3_Distant,_   > => "सन" ;
        
        <CFuture,Sg,Pers1,Masc   > => "गा" ;
        <CFuture,Sg,Pers1,Fem   > => "गय" ;
        <CFuture,Sg,Pers2_Casual,Masc   > => "गा" ;
        <CFuture,Sg,Pers2_Casual,Fem   > => "गय" ;
        <CFuture,Sg,Pers2_Respect,Masc   > => "गै" ;
        <CFuture,Sg,Pers2_Respect,Fem   > => "गै" ;
        <CFuture,Sg,Pers3_Near,Masc   > => "गा" ;
        <CFuture,Sg,Pers3_Near,Fem   > => "गय" ;
        <CFuture,Sg,Pers3_Distant,Masc  > => "गा" ;
        <CFuture,Sg,Pers3_Distant,Fem  > => "गय" ;
        <CFuture,Pl,Pers1,Masc   > => "गै" ;
        <CFuture,Pl,Pers1,Fem   > => "गयाण" ;
        <CFuture,Pl,Pers2_Casual,Masc   > => "गै" ;
        <CFuture,Pl,Pers2_Casual,Fem   > => "गयाण" ;
        <CFuture,Pl,Pers2_Respect,Masc   > => "गै" ;
        <CFuture,Pl,Pers2_Respect,Fem   > => "गयाण" ;
        <CFuture,Pl,Pers3_Near,Masc   > => "गै" ;
        <CFuture,Pl,Pers3_Near,Fem   > => "गयाण" ;
        <CFuture,Pl,Pers3_Distant,Masc  > => "गै" ;
        <CFuture,Pl,Pers3_Distant,Fem  > => "गयाण" 
    } ;
-}
 param
 
    
    VPPTense = 
	  VPPres
	  | VPPast 
	  | VPFutr  ;
	  --| VPPerf ;
      
   --PastTypes = Simple  | Habitual | Unknown ;
   -- Definitive and Nondefinitive
   --FutrTypes = Defin | NonDefin ;
   
    
    VPHTense = 
       VPGenPres  -- impf hum       nahim    "ी गो"
     | VPImpPast  -- impf Ta        nahim    "ी ौेनत"
	 | VPFut      -- fut            na/nahim "ी सहालल गो"
     | VPContPres -- stem raha hum  nahim    "ी ाम गोिनग"
     | VPContPast -- stem raha Ta   nahim    "ी ौास गोिनग"
	 | VPContFut
     | VPPerfPres -- perf hum       na/nahim "ी हावे गोने"
     | VPPerfPast -- perf Ta        na/nahim "ी हाद गोने"          
	 | VPPerfFut
	 | VPPerfPresCont
	 | VPPerfPastCont
	 | VPPerfFutCont
     | VPSubj     -- subj           na       "ी माय गो"
     ;
     
    VPHForm = 
       VPTense VPPTense Aspect Agr -- 9 * 12
     | VPReq
     | VPImp
     | VPReqFut
     | VPInf
     | VPStem
     ;
        
    VType = VIntrans | VTrans | VTransPost ;

  oper
{-    
	objVType : VType -> NPCase = \vt -> case vt of {
      VTrans => NPObj ;
      _ => NPC Nom
      } ;
-}
    VPH : Type = {
      s    : VerbForm => {inf : Str} ;
      obj  : {s : Str ; a : Agr} ; 
      subj : VType ;
      comp : Agr => Str;
--      inf : Str;
      ad  : Str;
      --embComp : Str ;
      --prog : Bool ;
      } ;
	
--	VPHSlash = VPH ** {c2 : Compl} ;

    Compl : Type = {s : Str ; c : VType} ;
--VF VTense Aspect Polarity NPerson Number Gender
   predV : Verb -> VPH = \verb -> {
      s = \\vf => 
	   case vf of {
	     VF t a pl p n g => { inf = verb.s ! VF t a pl p n g } ;
		 Root => { inf = verb.s ! Root} 
		 };
	    obj = {s = [] ; a = defaultAgr} ;
		subj = VIntrans ;
--		inf = verb.s ! Inf;
		ad = [];
  --      embComp = [];
--	prog = False ;
        comp = \\_ => []
      } ;

--    predVc : (Verb ** {c2,c1 : Str}) -> VPHSlash = \verb -> 
--    predV verb ** {c2 = {s = verb.c1 ; c = VTrans} } ;

    
{-
-------------------------
-- added for cauitives
   predVcc : (Verb **{c2:Compl}) -> VPHSlash = \verb ->
    predV verb ** {c2 = {s = "" ; c = VTrans} } ;
------------------------
	 
    raha : Gender -> Number -> Str = \g,n -> 
	   (mkAdj1 "रया").s ! n ! g ! Dir ;
    pya : Gender -> Number -> Str = \g,n -> 
	   (mkAdj1 "पया").s ! n ! g ! Dir ;	   

	cka : Gender -> Number -> Str = \g,n -> 
	  (mkAdj1 "छका").s ! n ! g ! Dir ;
	  
	hw : PPerson -> Number -> Str = \pp,n ->    
	 case <pp,n> of {
	 <Pers1,_> => "हौौाण";
	 <Pers2_Casual,Sg>    => "हौौयण";
	 <Pers2_Casual,Pl>    => "हौौ";
	 <Pers2_Respect,_>    => "हौौ";
	 <Pers3_Distant,Sg>    => "हौौै";
	 <Pers3_Distant,Pl>    => "हौन";
	 <Pers3_Near,Sg>    => "हौौै";
	 <Pers3_Near,Pl>    => "हौन"
	 
	 };
	 
	predAux : Aux -> VPH = \verb -> {
     s = \\vh => 
       let  

		 inf  = verb.inf ;
         part = verb.ppart ;

       in
       case vh of {
	     VPTense VPPres (Ag g n p) => {fin = copula CPresent n p g ; inf = part } ;
         VPTense VPPast (Ag g n p) => {fin = copula CPast n p g ; inf = part } ;
         VPTense VPFutr (Ag g n p) => {fin = copula CFuture n p g ; inf = part ++ hw p n  } ;
         VPStem => {fin = []  ; inf = "रह" };
		 _ => {fin = part ; inf = [] }
		 };
	  obj = {s = [] ; a = defaultAgr} ;
      subj = VIntrans ;
      inf = verb.inf;
	  ad = [];
      embComp = [];
      prog = False ;
      comp = \\_ => []
      } ;
    
  Aux = {
      inf,ppart,prpart : Str
    } ;

  auxBe : Aux = {
    inf  = "" ;
    ppart = "" ;
    prpart = ""
    } ;
    
  predProg : VPH -> VPH = \verb -> {
     s = \\vh => 
       case vh of {
         VPTense VPPres (Ag g n p) => {fin = copula CPresent n p g ; inf = (verb.s!VPTense VPPres (Ag g n p)).inf ++ pya g n} ;
         VPTense VPPast (Ag g n p) => {fin = copula CPast n p g ; inf = (verb.s!VPTense VPPres (Ag g n p)).inf ++ pya g n} ;
         VPTense VPFutr (Ag g n p) => {fin = copula CFuture n p g ; inf = (verb.s!VPTense VPPres (Ag g n p)).inf } ;
	 VPTense VPPerf (Ag g n p) => {fin = copula CPast n p g ; inf = (verb.s!VPTense VPPres (Ag g n p)).inf ++ raha g n } ;
         VPStem => {fin = []  ; inf = (verb.s!VPStem).inf };
		 _ => {fin = [] ; inf = [] }
		 };
      obj = verb.obj ;
      subj =  VIntrans ;
      inf = verb.inf;
      ad = verb.ad;
      embComp = verb.embComp;
      prog = True ;
      comp = verb.comp 
      } ;
 -}  	
  Clause : Type = {s : VPHTense => Polarity => Order => Str} ;
  mkClause : NP -> VPH -> Clause = \np,vp -> {
      s = \\vt,b,ord => 
        let 
          subjagr : NPCase * Agr = case vt of {
            VPImpPast => case vp.subj of {
--              VTrans     => <NPErg, vp.obj.a> ;
--              VTransPost => <NPErg, defaultAgr> ;
              _          => <NPC Dir, np.a>
              } ;
            _ => <NPC Dir, np.a>
            } ;
          subj = subjagr.p1 ;
          agr  = subjagr.p2 ;
		  n    = (fromAgr agr).n;
		  p    = (fromAgr agr).p;
		  g    = (fromAgr agr).g;
          vps  = case vt of {

				    VPGenPres  => vp.s !  VF NPast Simpl Imperf Pos p n g ;{-
					VPImpPast  => vp.s !  VPTense VPPast agr ;
				    
					VPFut      => case vp.prog of { True => {fin = (vp.s !  VPTense VPFutr agr).fin ; inf = (vp.s !  VPTense VPFutr agr).inf ++ hw p n} ;
					                                _    => vp.s !  VPTense VPFutr agr } ;
					VPContPres => 
					  {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ;
					VPContPast => 
					  {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ raha g n } ;
					VPContFut  => 
					  {fin = copula CFuture n p g ; inf = (vp.s ! VPStem).inf ++ raha g n ++ hw p n} ;
					VPPerfPres =>
					  {fin = copula CPresent n p g ; inf = (vp.s ! VPTense VPPerf agr).inf } ;  
					--  {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ cka g n } ;
					VPPerfPast => 
                      -- {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ cka g n } ;
		                        {fin = copula CPast n p g ; inf = (vp.s ! VPTense VPPerf agr).inf } ;  
					VPPerfFut  => 
					  {fin = copula CFuture n p g ; inf = (vp.s ! VPTense VPPerf agr).inf  ++ hw p n } ;
					VPPerfPresCont => 
					  {fin = copula CPresent n p g ; inf = (vp.s ! VPTense VPPres agr).inf ++ raha g n } ;					
					VPPerfPastCont => 
					  {fin = copula CPast n p g ; inf = (vp.s ! VPTense VPPres agr).inf ++ raha g n } ;					
					VPPerfFutCont => 
					  {fin = copula CFuture n p g ; inf = (vp.s ! VPTense VPPres agr).inf ++ raha g n  ++ hw p n } ;					
					VPSubj   => case vp.prog of { True => {fin = (vp.s !  VPTense VPFutr agr).inf ++ hw p n ; inf = "ढायद" } ;
                    -}
					                                _    => { inf = "ढायद" } } 
                   
					};
					
		    
          quest =
            case ord of
              { ODir => [];
                OQuest => "कय" }; 
		  na =
            case b of
              { Pos => [];
                Neg => "ना" };
           nahim =
            case b of 
              { Pos => [];
                Neg => "नयण" };
        in
		case vt of {
		VPSubj => quest ++ np.s ! subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! np.a  ++ na ++  vps.inf ;
		_      => quest ++ np.s ! subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! np.a  ++ nahim  ++  vps.inf};

  } ;

{-
  mkSClause : Str -> Agr -> VPH -> Clause =
    \subj,agr,vp -> {
      s = \\t,b,ord => 
        let 
		  n    = (fromAgr agr).n;
		  p    = (fromAgr agr).p;
		  g    = (fromAgr agr).g;
          vps  = case t of {
                    VPGenPres  => vp.s !  VPTense VPPres agr ;
					VPImpPast  => vp.s !  VPTense VPPast agr ;
					VPFut      => vp.s !  VPTense VPFutr agr ;
					VPContPres => 
					  {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ pya g n  } ;
					VPContPast => 
					  {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ pya g n } ;
					VPContFut  => 
					  {fin = copula CFuture n p g  ; inf = (vp.s ! VPStem).inf ++ pya g n ++ hw p n  } ;
					VPPerfPres => 
					  {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ cka g n } ;
					VPPerfPast => 
                      {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ cka g n } ;
					VPPerfFut  => 
					  {fin = copula CFuture n p g ; inf = (vp.s ! VPStem).inf ++ cka g n ++ hw p n } ;
					VPPerfPresCont => 
					 {fin = copula CPresent n p g ; inf = (vp.s ! VPStem).inf ++ pya g n } ; 
					VPPerfPastCont => 
					  {fin = copula CPast n p g ; inf = (vp.s ! VPStem).inf ++ pya g n } ; 
					VPPerfFutCont => 
					  {fin = copula CFuture n p g ; inf = (vp.s ! VPStem).inf ++ pya g n ++ hw p n } ;
					VPSubj   => {fin = insertSubj p (vp.s ! VPStem).inf ; inf = "डायद"  }
                    
					};

		  quest =
            case ord of
              { ODir => [];
                OQuest => "कय" }; 
		  na =
            case b of
              { Pos => [];
                Neg => "ना" };
          nahim =
            case b of 
              { Pos => [];
                Neg => "नयण" };		
        in
		case t of {
		VPSubj => quest ++ subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! agr  ++ na ++  vps.inf ++ vps.fin ++ vp.embComp;
		_      => quest ++ subj ++ vp.obj.s ++ vp.ad ++ vp.comp ! agr  ++ nahim ++  vps.inf ++ vps.fin ++ vp.embComp};
    } ;
    
    insertSubj : PPerson -> Str -> Str = \p,s -> 
      case p of { Pers1 => s ++ "ौण" ; _ => s ++ "ै"};
     
    insertObj : (Agr => Str) -> VPH -> VPH = \obj1,vp -> {
     s = vp.s ;
     obj = vp.obj ;
     subj = vp.subj ;
	 inf = vp.inf;
	 ad = vp.ad;
     embComp = vp.embComp;
     prog = vp.prog ;
     comp = \\a =>    vp.comp ! a  ++ obj1 ! a 
     } ;
     insertVV : Str -> VPH -> Str -> VPH -> VPH = \obj1,vp,emb,vp2 -> {
     s = vp.s ;
--     obj = vp.obj ;
     obj = vp2.obj ;
     subj = vp.subj ;
	 inf = vp.inf;
	 ad = vp.ad;
     embComp = vp.embComp ++ emb; -- this should be covered in urdu as well
     prog = vp.prog ;
     comp = \\a =>    vp.comp ! a  ++ obj1  
     } ;
     
    insertObj2 : (Str) -> VPH -> VPH = \obj1,vp -> {
     s = vp.s;
     obj = vp.obj ;
     subj = vp.subj ;
	 inf = vp.inf;
	 ad = vp.ad;
     embComp = vp.embComp ++ obj1;
     prog = vp.prog ;
     comp = vp.comp
     
     } ;
	 
    insertObjc : (Agr => Str) -> VPHSlash -> VPHSlash = \obj,vp -> 
    insertObj obj vp ** {c2 = vp.c2} ;
    insertObjc2 : Str -> VPHSlash -> VPHSlash = \obj,vp -> 
    insertObj2 obj vp ** {c2 = vp.c2} ;

	infVP : Bool -> VPH -> Agr -> Str = \isAux,vp,a ->
     vp.obj.s ++ vp.inf ++ vp.comp ! a ;
    infVV : Bool -> VPH -> Str = \isAux,vp -> 
--      case isAux of {False =>  vp.obj.s ++ (vp.comp ! (toAgr Sg Pers1 Masc)) ++ vp.inf  ; True => vp.obj.s ++ (vp.comp ! (toAgr Sg Pers1 Masc)) ++ (vp.s ! VPImp).fin  }; -- need to be checked and should be covered in urdu as well
        case isAux of {False =>  (vp.comp ! (toAgr Sg Pers1 Masc)) ++ vp.inf  ; True => (vp.comp ! (toAgr Sg Pers1 Masc)) ++ (vp.s ! VPImp).fin  }; -- need to be checked and should be covered in urdu as well
    infV2V : Bool -> VPH -> Str = \isAux,vp -> 
--      case isAux of {False =>  vp.obj.s ++ (vp.comp ! (toAgr Sg Pers1 Masc)) ++ vp.inf ++ "दय"  ; True => vp.obj.s ++ (vp.comp ! (toAgr Sg Pers1 Masc)) ++ (vp.s ! VPImp).fin  ++ "दय"}; -- need to be checked and should be covered in urdu as well
      case isAux of {False =>  (vp.comp ! (toAgr Sg Pers1 Masc)) ++ vp.inf ++ "दय"  ; True => (vp.comp ! (toAgr Sg Pers1 Masc)) ++ (vp.s ! VPImp).fin  ++ "दय"}; -- need to be checked and should be covered in urdu as well  

    insertObject : NP -> VPHSlash -> VPH = \np,vps -> {
      s = vps.s ;
      obj = case np.isPron of { 
            False => {s =  vps.obj.s  ++ np.s ! objVType vps.c2.c ++ vps.c2.s ; a = np.a} ;
	    _     => {s =  vps.obj.s  ++ np.s ! objVType vps.c2.c ; a = np.a}
	    };
      subj = vps.c2.c ;
	  inf = vps.inf;
	  ad = vps.ad;
      embComp = vps.embComp;
      prog = vps.prog ;
      comp = vps.comp
      } ;
	  
	insertObjPre : (Agr => Str) -> VPHSlash -> VPH = \obj,vp -> {
     s = vp.s ;
     obj = vp.obj ;
     inf = vp.inf ;
     subj = vp.subj ;
	 ad = vp.ad ;
     embComp = vp.embComp;
     prog = vp.prog ;
     comp = \\a =>   obj ! a  ++ vp.c2.s ++ vp.comp ! a 
    } ;

    insertAdV : Str -> VPH -> VPH = \ad,vp -> {
     s = vp.s ;
     obj = vp.obj ;
     inf = vp.inf ;
	 subj = vp.subj;
     ad  = vp.ad ++ ad ;
     embComp = vp.embComp;
     prog = vp.prog ;
     comp = vp.comp
    } ;
	conjThat : Str = "कह" ;
    checkPron : NP -> Str -> Str = \np,str ->  case (np.isPron) of {
                                True => np.s ! NPC Obl;
                                False => np.s ! NPC Obl ++ str} ;
		
    insertEmbCompl : VPH -> Str -> VPH = \vp,emb -> {
     s = vp.s ;
     obj = vp.obj ;
     inf = vp.inf ;
     subj = vp.subj;
     ad  = vp.ad;
     embComp = vp.embComp ++ emb;
     prog = vp.prog ;
     comp = vp.comp
    } ;
    
    insertTrans : VPH -> VType -> VPH = \vp,vtype -> {
     s = vp.s ;
     obj = vp.obj ;
     inf = vp.inf ;
     subj = case vtype of {VIntrans => VTransPost ; VTrans => VTrans ; _ => vtype} ; -- still some problem not working properly
     ad  = vp.ad;
     embComp = vp.embComp ;
     prog = vp.prog ;
     comp = vp.comp
    } ;
-}
}
