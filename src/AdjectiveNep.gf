concrete AdjectiveNep of Adjective = CatNep ** open ResNep, Prelude in {

  flags coding = utf8;
  lin
    
    -- ???? problem covering all the rules from the book

    PositA a = a ;
	
    ComparA a np = {
      s = \\n, g => np.s ! NPC Nom ++ "Bnx:da" ++ a.s ! n ! g ;
      } ;
    
    {-
    ComparA a np = {
      s = \\n, g => case np.a of {
        Ag _ Sg _ => np.s ! NPC Nom ++ "Bnx:da" ++ a.s ! n ! g ;  -- भन्दा
        _         => np.s ! NPC Nom ++ "Bnx:da" ++ a.s ! n ! g
        }
      } ;
     -}
     
---- $SuperlA$ belongs to determiner syntax in $Noun$.
  
    ComplA2 a np = {
      s = \\n,g  => np.s ! NPC Nom ++ a.c2 ++ a.s ! n ! g  ; 
      } ;

    ReflA2 a = { 
      s = \\n, g => "APE" ++ a.c2 ++ a.s ! n ! g ; -- आफै सँग
      } ;
      
    UseA2 a = a ;
    
    UseComparA a = {
      s = \\n, g => "HlI" ++ a.s ! n ! g ; -- अली
      } ;     
    
    CAdvAP cadv ap np = {
	  s = \\n,g => cadv.s ++ ap.s ! n ! g ++ cadv.p ++  np.s ! NPC Nom ;
	  };    
    
    AdjOrd ord =  { s = \\_,_ => ord.s ; };        
    
    SentAP ap sc = {
      s = \\n,g => ap.s ! n ! g ++ sc.s ; 
      } ;

    AdAP ada ap = {
      s = \\n,g => ada.s ++ ap.s ! n ! g ;
      } ;
      
    -- Adv is overritten in CatNep {s : Gender => Str} ;
    AdvAP ap adv = {
      s = \\n,g => adv.s ++ ap.s ! n ! g ;
      } ;
      
}
