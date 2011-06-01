concrete PhraseNep of Phrase = CatNep ** open Prelude, ResNep in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = {s = qs.s ! QDir} ;
--    UttImpSg pol imp = {s = pol.s ++ imp.s ! contrNeg True pol.p ! ImpF Sg False} ;
--    UttImpPl pol imp = {s = pol.s ++ imp.s ! contrNeg True pol.p ! ImpF Pl False} ;
--    UttImpPol pol imp = {s = pol.s ++ imp.s ! contrNeg True pol.p ! ImpF Sg True} ;

    UttIP ip = {s = ip.s ! Nom} ; --- Acc also
    UttIAdv iadv = iadv ;
    UttNP np = {s = np.s ! NPC Nom} ;
--    UttVP vp = {s = infVP False vp (agrP3 Masc Sg)} ;
    UttAdv adv = {s = adv.s } ;
	UttCN cn = {s = cn.s ! Sg ! Nom};
    UttCard n = n ;
    UttAP ap = {s = ap.s ! Sg ! Masc } ;

    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ; ---

    NoVoc = {s = []} ;
    VocNP np = {s = np.s ! NPC Nom} ;

}
