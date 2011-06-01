concrete StructuralNep of Structural = CatNep ** 
  open MorphoNep, ParadigmsNep, Prelude, NounNep in {

  flags optimize=all ;
  coding = utf8;

  lin

    above_Prep = mkPrep "maTi" ; -- माथि
    after_Prep = mkPrep "pci" ; -- पछि
    all_Predet = ss "sbE" ; -- सबै (adj)
    almost_AdA = mkAdA "JNx:xE" ;
    almost_AdN = mkAdN "JNx:xE" ; -- JNx:xE (Adv) झण्डै
    although_Subj = ss "tEpni" ; -- तैपनि (conj)
    always_AdV = mkAdV "sDEV" ; -- सधैँ
    and_Conj = sd2 [] "r" ** {n = Pl} ;
    because_Subj = ss "kinBne" ; -- किनभने
    before_Prep = mkPrep "HGi" ; -- अघि 
    behind_Prep = mkPrep "pci" ; -- पछि
    between_Prep = mkPrep "biC" ; -- बिच
    both7and_DConj = sd2 "dwwyN" "tE" ** {n = Pl} ;
    but_PConj = ss "tr" ; -- तर
    by8agent_Prep = mkPrep "lai:" ; -- 
    by8means_Prep = mkPrep "le" ; 
    can8know_VV,can_VV = mkV "skx:nu" ** { isAux = True} ;
    during_Prep = mkPrep "prx:yanx:t" ; -- पर्यान्त
    either7or_DConj = sd2 "HTva" "ya" ** {n = Sg} ; -- अथवा , या
    everybody_NP =  MassNP (UseN (regN "svw jana")) ; -- not a good way coz need to include NounNep (सवौ जाना)
    --every_Det = mkDet "hr" "hr" "hr" "hr" Sg;
    everything_NP = MassNP (UseN (regN "harek kura")) ; -- हारेक कुरा
    everywhere_Adv = mkAdv "jata ttE" ; -- जाता ततै
    --few_Det = mkDet "kch'" "kch'" "kch'" "kch'" Pl ;
    first_Ord = {s = "pehla" ; n = Sg} ; --DEPRECATED
    for_Prep = mkPrep "lagi" ; -- लागि
    from_Prep = mkPrep "baq" ; -- बाट
    he_Pron = mkPron "f" "fsx:laI:" "fsx:le" "fsx:laI:" "fsbaq" "fsx:ma" "" Sg  Masc Pers3_L; --उ, उस्लाई, उस्ले, उस्लाई, उसवाट, उस्मा
    here_Adv = mkAdv "yhaV" ; -- यहाँ
    here7to_Adv = mkAdv ["yhaV smx:m"] ; -- यहाँ सम्म
    here7from_Adv = mkAdv ["yhaV baq"] ; -- यहाँ बाट
    how_IAdv = ss "ksrI" ; -- कसरी
    how8much_IAdv  = ss "ktI" ; -- कती
    --how8many_IDet = makeIDet "kynE" "kyny" Pl ;  
    if_Subj = ss "ydi" ; -- यदि
    in8front_Prep = mkPrep "samu" ; -- सामु  
    i_Pron = mkPron "m" "" Sg Masc Pers1;
    in_Prep = mkPrep "ma" ; -- मा
    it_Pron  = mkPron "yo" "yslaI:" "yesle" "yslaI:" "ysbaq" "ysma" "" Sg Masc Pers3_L; -- यो, यसलाई, यसले, यसलाई, यसबाट, यसमा
    less_CAdv = {s = "km" ; p = "Bnx:da"} ; -- ?? NOT CLEAR
    --many_Det = mkDet "bht zyadh" "bht zyadh" "bht zyadh" "bht zyadh" Pl ;
    more_CAdv = {s = "km" ; p = "Bnx:da" } ;
    most_Predet = ss "jx:yadE" ; -- ज्यादै
    --much_Det = mkDet "bht" "bht" "bht" "bht" Sg  ;
--  must_VV = {
--    s = table {
--      VVF VInf => ["have to"] ;
--      VVF VPres => "must" ;
--      VVF VPPart => ["had to"] ;
--      VVF VPresPart => ["having to"] ;
--      VVF VPast => ["had to"] ;      --# notpresent
--      VVPastNeg => ["hadn't to"] ;      --# notpresent
--      VVPresNeg => "mustn't"
--      } ;
--    isAux = True
--    } ;
-----b  no_Phr = ss "no" ;
  
    no_Utt = ss "hoI:n" ; -- होईन
    on_Prep = mkPrep "ma" ; -- मा
    --one_Quant = demoPN "ak" "ak" "ak"  ; -- DEPRECATED
    only_Predet = ss "matx:r" ; -- मात्र
    or_Conj = sd2 [] "HTva" ** {n = Sg} ; -- अथवा
    otherwise_PConj = ss "Hnx:yTa" ; -- अन्यथा
    --part_Prep = mkPrep "hSh" ;
    please_Voc = ss "kRpya" ; -- कृपया
    --possess_Prep = mkPrep "da" ;
    quite_Adv = ss "a:kdm" ; -- एकदम
    she_Pron = mkPron "fnI" "" Sg Fem Pers3_M ; -- उनी
    so_Ada = mkAdA "ys karN" ; -- यस कारण ???? NEED TO CHECK
    somebody_NP = MassNP (UseN (regN "kohI")); -- कोही
    --someSg_Det = mkDet "kch'" "kch'" "kch'" "kch'" Sg ;
    --somePl_Det = mkDet "kch'" "kch'" "kch'" "kch'" Pl ;
    --something_NP = MassNP (UseN (MorphoPnb.mkN11 "kwy XE"));
    somewhere_Adv = mkAdv "khIM" ; -- कहीं
    -- that_Quant = demoPN "wh" "as" "an" ;
    that_Subj = ss "tx:yo"; -- त्यो
    there_Adv = mkAdv "tx:yhaV" ; -- त्यहाँ
    there7to_Adv = mkAdv "tx:yhaV smx:m" ; -- त्यहाँ सम्म
    there7from_Adv = mkAdv "tx:yhaV baq" ; -- त्यहाँ बाट
    therefore_PConj = ss "Hth:" ; -- अतः
    they_Pron = mkPron "fnIhru" "" Pl Masc Pers3_L ; -- उनिहरु
    --this_Quant = demoPN "yh" "as" "an";      
    through_Prep = mkPrep "marx:Pt" ; -- मार्फत
    too_Ada = mkAdA "pni" ; -- पनि
    to_Prep = ss "samx:m" ; --TODO
    under_Prep = mkPrep "Hnx:trx:gt" ; -- अन्तर्गत
    very_Ada = mkAdA "DerE" ; -- धेरै
    want_VV = mkV "Cahnu" ** { isAux = False} ;
  
    we_Pron = mkPron "hamIhru" "" Pl Masc Pers1 ; -- हामीहरु
    whatSg_IP = mkIP "ke" "ke" "ke" "" Sg ;
    whatPl_IP = mkIP "ke" "ke" "ke" "" Pl ;
    when_IAdv = ss "khile" ; -- कहिले
    when_Subj = ss "khile" ;
    where_IAdv = ss "khaV" ; -- कहाँ
    which_IQuant = {s = table {Sg => "kun" ; Pl => "kun" } }; -- कुन
    whichPl_IDet = {s = "kun" ; n = Sg} ;
    whichSg_IDet = {s = "kun" ; n = Pl} ;
    whoSg_IP = mkIP "ko" "kaslaI:" "ksko" "" Sg ; 
    whoPl_IP = mkIP "ko" "kaslaI:" "ksko" "" Pl ;
    why_IAdv = ss "kin" ; -- किन
    without_Prep = mkPrep "vina" ; -- विना
    with_Prep = mkPrep "sVg" ; -- सँग
    yes_Phr = ss "hjur" ; -- हजुर
    yes_Utt = ss "hjur" ;  
    -- ?? you inflect ta, timi, tapai and its plural
    youSg_Pron = mkPron "timI" "" Sg Masc Pers2_M ; -- तिमी
    youPl_Pron = mkPron "timIhru" "" Pl Masc Pers2_M ; -- तिमीहरु
    youPol_Pron = mkPron "tpaI:" "" Sg Masc Pers2_H  ; -- तपाई  
    --no_Quant =  demoPN " kwy nhyN" "kwy nhyN" "kwy nhyN" ; 
    not_Predet = {s="hEn"} ; -- हैन
    if_then_Conj = sd2 "ydi" "Bne" ** {n = Sg} ; -- यदि भने
    at_least_AdN = mkAdN "kmsekm" ; -- कमसेकम
    --at_most_AdN = mkAdN "at most" ;
    --nothing_NP = MassNP (UseN (MorphoPnb.mkN11 "kch' nyN" )); 
    --nobody_NP = MassNP (UseN (MorphoPnb.mkN11 "kwy nhyN"));  
    except_Prep = mkPrep "bahek" ; -- बाहेक
    --as_CAdv = C.mkCAdv "as" "as" ;
    as_CAdv = {s = "srI" ; p = "srI"} ;  -- सरी
    have_V2 = mkV2 (mkV "rakh'na") "" ;
    language_title_Utt = ss "nepalI" ;
}

