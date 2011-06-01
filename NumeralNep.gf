concrete NumeralNep of Numeral = CatNep ** open ResNep, Prelude in {
-- By Harald Hammarstroem
-- Modification for Punjabi by Shafqat Virk
 flags coding=utf8 ;


param DForm = unit | ten ;
param DSize = sg | r2 | r3 | r4 | r5 | r6 | r7 | r8 | r9 ;
param Size = singl | less100 | more100 ; 

oper LinDigit = {s : DForm => Str ; size : DSize ; n : Number} ;


lincat Dig = { s:Str ; n : Number};
lincat Digit = LinDigit ;
lincat Sub10 = {s : DForm => Str ; size : DSize ; n : Number} ;
lincat Sub100 = {s : Str ; size : Size ; n : Number} ;
lincat Sub1000 = {s : Str ; s2 : Str ; size : Size ; n : Number } ; 
lincat Sub1000000 = { s : Str ; n : Number } ;

lin num x0 = 
    {s = table {
          NCard =>   x0.s ; 
          NOrd =>  x0.s ++ "ौं" -- need to use mkOrd x0.s but it gives linking error 
          };
       n = x0.n
    } ;


oper mkNum : Str -> Str -> DSize -> LinDigit = 
  \do -> \bis -> \sz ->  
  {s = table {unit => do ; ten => bis } ; 
   size = sz ; n = Pl} ;

lin n2 = mkNum "दुि" "बिस" r2 ;
lin n3 = mkNum "तिन" "तिस" r3 ;
lin n4 = mkNum "छार" "छालयस" r4 ;
lin n5 = mkNum "पानछ" "पछास" r5 ;
lin n6 = mkNum "छh'" "साथh'" r6 ; 
lin n7 = mkNum "सात" "सतर" r7; 
lin n8 = mkNum "आथh'" "ासय" r8;
lin n9 = mkNum "नौ" "नौै" r9 ;

oper mkR : Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> Str -> DSize => Str = \a1 -> \a2 -> \a3 -> \a4 -> \a5 -> \a6 -> \a7 -> \a8 -> \a9 -> table {
  sg => a1 + "ह" ;
  r2 => a2 + "यस" ;
  r3 => a3 + "तयस" ;
  r4 => a4 + "ालयस" ;
  r5 => a5 + "न" ;
  r6 => a6 + "साथh'" ;
  r7 => a7 + "हतर" ;
  r8 => a8 + "ासय" ;
  r9 => a9 + "ानौै"
} ;

oper rows : DSize => DSize => Str = table {
  sg => mkR "गयार" "ाक" "ाकत" "ाकत" "ाकयाौ" "ाक" "ाक" "ाकय" "ाकय" ; 
  r2 => mkR "बार" "बाय" "बात" "बाय" "बाौ" "बा" "बा" "बाय" "ब" ;
  r3 => mkR "तयर" "तय" "तयन" "तनत" "तरप" "तरय" "त" "तर" "तर" ;
  r4 => mkR "छौद" "छौब" "छौन" "छौा" "छौौ" "छौन" "छौह" "छौर" "छौर" ;
  r5 => mkR "पनदर" "पछय" "पयन" "पनता" "पछप" "पयन" "पह" "पछ" "पछ" ;
  r6 => mkR "सौल" "छh'ब" "छh'त" "छh'य" "छh'प" "छh'या" "छh'" "छh'य" "छh'य" ;
  r7 => mkR "सतर" "सता" "सयन" "सनत" "सताौ" "सता" "सर" "सत" "सता" ;
  r8 => mkR "ाथh'ार" "ाथh'ाय" "ाृ" "ाृत" "ाथh'ाौ" "ाृ" "ाथh'" "ाथh'" "ाथh'" ; 
  r9 => table {sg => "ानयस" ; r2 => "ानतयस" ; r3 => "ानतालयस" ; 
               r4 => "ानछास" ; r5 => "ानसतh'" ; r6 => "ानहतर" ; 
               r7 => "ानासय" ; 
               r8 => "ानानौै" ; r9 => "ननानौै" } 
} ;

oper ss : Str -> {s : Str} = \s -> {s = s} ;

lin pot01 = {s = table {unit => "ायक" ; _ => "दमय" } ; size = sg ; n = Sg} ;
lin pot0 d = d ; 
lin pot110 = {s = "दस" ; size = less100 ; n = Pl} ; 
lin pot111 = {s = rows ! sg ! sg ; size = less100 ; n = Pl} ;
lin pot1to19 d = {s = rows ! d.size ! sg ; size = less100 ; n = d.n} ;
lin pot0as1 n = {s = n.s ! unit ; size = table {sg => singl ; _ => less100} ! n.size ; n = n.n } ;

lin pot1 d = {s = d.s ! ten ; size = less100 ; n = d.n} ;
lin pot1plus d e = {s = rows ! e.size ! d.size ; size = less100 ; n = d.n} ;

lin pot1as2 n = {s = n.s ; s2 = "दमय" ; size = n.size ; n = n.n} ;
lin pot2 d = {s = (mksau (d.s ! unit) d.size) ; 
              s2 = d.s ! unit ++ "लाकh'" ; size = more100 ; n = d.n} ;
lin pot2plus d e = 
  {s = (mksau (d.s ! unit) d.size) ++ e.s ; 
   s2 = (d.s ! unit) ++ "लाकh'" ++ (mkhazar e.s e.size) ; 
   size = more100 ; n = d.n} ;

lin pot2as3 n = {s = n.s ; n = n.n} ;
lin pot3 n = {s = table { singl => ekhazar ;
                          less100 => n.s ++ "हषार" ; 
                          more100 => n.s2 } ! n.size ; n = n.n} ;
lin pot3plus n m = 
  {s = table {singl => ekhazar ;
              less100 => n.s ++ "हषार" ; 
              more100 => n.s2 } ! n.size ++ m.s ; n = n.n} ;

lin D_0 = { s = "N0" ; n = Sg};
lin D_1 = { s = "N1" ; n = Sg};
lin D_2 = { s = "N2" ; n = Pl};
lin D_3 = { s = "N3" ; n = Pl};
lin D_4 = { s = "N4" ; n = Pl};
lin D_5 = { s = "N5" ; n = Pl};
lin D_6 = { s = "N6" ; n = Pl};
lin D_7 = { s = "N7" ; n = Pl};
lin D_8 = { s = "N8" ; n = Pl};
lin D_9 = { s = "N9" ; n = Pl};
lin IDig d = { s = \\_ => d.s ; n = d.n} ;
lin IIDig d dg = { s = \\df => Prelude.glue (dg.s ! df) d.s ; n = Pl }; 

oper ekhazar : Str = variants {"हषार" ; "ायक" ++ "हषार"} ; 
oper mkhazar : Str -> Size -> Str = \s -> \sz -> table {singl => ekhazar ; _ => s ++ "हषार"} ! sz ;
oper mksau : Str -> DSize -> Str = \s -> \sz -> table {sg => "ायक" ++ "सौ" ; _ => s ++ "सौ"} ! sz ;
}
