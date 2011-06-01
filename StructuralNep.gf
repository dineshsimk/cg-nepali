concrete StructuralNep of Structural = CatNep ** 
  open MorphoNep, ParadigmsNep, Prelude, NounNep in {

  flags optimize=all ;
  coding = utf8;

  lin

    above_Prep = mkPrep "माथि" ; -- माथि
    after_Prep = mkPrep "पछि" ; -- पछि
    all_Predet = ss "सबै" ; -- सबै (adj)
    almost_AdA = mkAdA "झण्डै" ;
    almost_AdN = mkAdN "झण्डै" ; -- JNx:xE (Adv) झण्डै
    although_Subj = ss "तैपनि" ; -- तैपनि (conj)
    always_AdV = mkAdV "सधैँ" ; -- सधैँ
    and_Conj = sd2 [] "र" ** {n = Pl} ;
    because_Subj = ss "किनभने" ; -- किनभने
    before_Prep = mkPrep "अघि" ; -- अघि 
    behind_Prep = mkPrep "पछि" ; -- पछि
    between_Prep = mkPrep "बिच" ; -- बिच
    both7and_DConj = sd2 "दौौयण" "तै" ** {n = Pl} ;
    but_PConj = ss "तर" ; -- तर
    by8agent_Prep = mkPrep "लाइ" ; -- 
    by8means_Prep = mkPrep "ले" ; 
    can8know_VV,can_VV = mkV "सक्नु" ** { isAux = True} ;
    during_Prep = mkPrep "पर्यान्त" ; -- पर्यान्त
    either7or_DConj = sd2 "अथवा" "या" ** {n = Sg} ; -- अथवा , या
    everybody_NP =  MassNP (UseN (regN "सवौ जाना")) ; -- not a good way coz need to include NounNep (सवौ जाना)
    --every_Det = mkDet "हर" "हर" "हर" "हर" Sg;
    everything_NP = MassNP (UseN (regN "हारेक कुरा")) ; -- हारेक कुरा
    everywhere_Adv = mkAdv "जाता ततै" ; -- जाता ततै
    --few_Det = mkDet "कछh'" "कछh'" "कछh'" "कछh'" Pl ;
    first_Ord = {s = "पेहला" ; n = Sg} ; --DEPRECATED
    for_Prep = mkPrep "लागि" ; -- लागि
    from_Prep = mkPrep "बाट" ; -- बाट
    he_Pron = mkPron "उ" "उस्लाई" "उस्ले" "उस्लाई" "उसबाट" "उस्मा" "" Sg  Masc Pers3_L; --उ, उस्लाई, उस्ले, उस्लाई, उसवाट, उस्मा
    here_Adv = mkAdv "यहाँ" ; -- यहाँ
    here7to_Adv = mkAdv ["यहाँ सम्म"] ; -- यहाँ सम्म
    here7from_Adv = mkAdv ["यहाँ बाट"] ; -- यहाँ बाट
    how_IAdv = ss "कसरी" ; -- कसरी
    how8much_IAdv  = ss "कती" ; -- कती
    --how8many_IDet = makeIDet "कयनै" "कयनय" Pl ;  
    if_Subj = ss "यदि" ; -- यदि
    in8front_Prep = mkPrep "सामु" ; -- सामु  
    i_Pron = mkPron "म" "" Sg Masc Pers1;
    in_Prep = mkPrep "मा" ; -- मा
    it_Pron  = mkPron "यो" "यसलाई" "येसले" "यसलाई" "यसबाट" "यसमा" "" Sg Masc Pers3_L; -- यो, यसलाई, यसले, यसलाई, यसबाट, यसमा
    less_CAdv = {s = "कम" ; p = "भन्दा"} ; -- ?? NOT CLEAR
    --many_Det = mkDet "बहत षयादह" "बहत षयादह" "बहत षयादह" "बहत षयादह" Pl ;
    more_CAdv = {s = "कम" ; p = "भन्दा" } ;
    most_Predet = ss "ज्यादै" ; -- ज्यादै
    --much_Det = mkDet "बहत" "बहत" "बहत" "बहत" Sg  ;
--  must_VV = {
--    s = table {
--      VVF VInf => ["हावे तो"] ;
--      VVF VPres => "मुसत" ;
--      VVF VPPart => ["हाद तो"] ;
--      VVF VPresPart => ["हाविनग तो"] ;
--      VVF VPast => ["हाद तो"] ;      --# notpresent
--      VVPastNeg => ["हादn'त तो"] ;      --# notpresent
--      VVPresNeg => "मुसतn'त"
--      } ;
--    isAux = True
--    } ;
-----b  no_Phr = ss "नो" ;
  
    no_Utt = ss "होईन" ; -- होईन
    on_Prep = mkPrep "मा" ; -- मा
    --one_Quant = demoPN "ाक" "ाक" "ाक"  ; -- DEPRECATED
    only_Predet = ss "मात्र" ; -- मात्र
    or_Conj = sd2 [] "अथवा" ** {n = Sg} ; -- अथवा
    otherwise_PConj = ss "अन्यथा" ; -- अन्यथा
    --part_Prep = mkPrep "हशह" ;
    please_Voc = ss "कृपया" ; -- कृपया
    --possess_Prep = mkPrep "दा" ;
    quite_Adv = ss "a:कदम" ; -- एकदम
    she_Pron = mkPron "उनी" "" Sg Fem Pers3_M ; -- उनी
    so_Ada = mkAdA "यस कारण" ; -- यस कारण ???? NEED TO CHECK
    somebody_NP = MassNP (UseN (regN "कोही")); -- कोही
    --someSg_Det = mkDet "कछh'" "कछh'" "कछh'" "कछh'" Sg ;
    --somePl_Det = mkDet "कछh'" "कछh'" "कछh'" "कछh'" Pl ;
    --something_NP = MassNP (UseN (MorphoPnb.mkN11 "कौय ढै"));
    somewhere_Adv = mkAdv "कहीं" ; -- कहीं
    -- that_Quant = demoPN "ौह" "ास" "ान" ;
    that_Subj = ss "त्यो"; -- त्यो
    there_Adv = mkAdv "त्यहाँ" ; -- त्यहाँ
    there7to_Adv = mkAdv "त्यहाँ सम्म" ; -- त्यहाँ सम्म
    there7from_Adv = mkAdv "त्यहाँ बाट" ; -- त्यहाँ बाट
    therefore_PConj = ss "अतः" ; -- अतः
    they_Pron = mkPron "उनीहरु" "" Pl Masc Pers3_L ; -- उनिहरु
    --this_Quant = demoPN "यह" "ास" "ान";      
    through_Prep = mkPrep "मार्फत" ; -- मार्फत
    too_Ada = mkAdA "पनि" ; -- पनि
    to_Prep = ss "साम्म" ; --TODO
    under_Prep = mkPrep "अन्तर्गत" ; -- अन्तर्गत
    very_Ada = mkAdA "धेरै" ; -- धेरै
    want_VV = mkV "चाहनु" ** { isAux = False} ;
  
    we_Pron = mkPron "हामीहरु" "" Pl Masc Pers1 ; -- हामीहरु
    whatSg_IP = mkIP "के" "के" "के" "" Sg ;
    whatPl_IP = mkIP "के" "के" "के" "" Pl ;
    when_IAdv = ss "कहिले" ; -- कहिले
    when_Subj = ss "कहिले" ;
    where_IAdv = ss "कहाँ" ; -- कहाँ
    which_IQuant = {s = table {Sg => "कुन" ; Pl => "कुन" } }; -- कुन
    whichPl_IDet = {s = "कुन" ; n = Sg} ;
    whichSg_IDet = {s = "कुन" ; n = Pl} ;
    whoSg_IP = mkIP "को" "कासलाई" "कसको" "" Sg ; 
    whoPl_IP = mkIP "को" "कासलाई" "कसको" "" Pl ;
    why_IAdv = ss "किन" ; -- किन
    without_Prep = mkPrep "विना" ; -- विना
    with_Prep = mkPrep "सँग" ; -- सँग
    yes_Phr = ss "हजुर" ; -- हजुर
    yes_Utt = ss "हजुर" ;  
    -- ?? you inflect ta, timi, tapai and its plural
    youSg_Pron = mkPron "तिमी" "" Sg Masc Pers2_M ; -- तिमी
    youPl_Pron = mkPron "तिमीहरु" "" Pl Masc Pers2_M ; -- तिमीहरु
    youPol_Pron = mkPron "तपाई" "" Sg Masc Pers2_H  ; -- तपाई  
    --no_Quant =  demoPN " कौय नहयण" "कौय नहयण" "कौय नहयण" ; 
    not_Predet = {s="हैन"} ; -- हैन
    if_then_Conj = sd2 "यदि" "भने" ** {n = Sg} ; -- यदि भने
    at_least_AdN = mkAdN "कमसेकम" ; -- कमसेकम
    --at_most_AdN = mkAdN "ात मोसत" ;
    --nothing_NP = MassNP (UseN (MorphoPnb.mkN11 "कछh' नयण" )); 
    --nobody_NP = MassNP (UseN (MorphoPnb.mkN11 "कौय नहयण"));  
    except_Prep = mkPrep "बाहेक" ; -- बाहेक
    --as_CAdv = C.mkCAdv "ास" "ास" ;
    as_CAdv = {s = "सरी" ; p = "सरी"} ;  -- सरी
    have_V2 = mkV2 (mkV "राकh'ना") "" ;
    language_title_Utt = ss "नेपाली" ;
}

