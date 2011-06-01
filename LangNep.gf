--# -path=.:../abstract:../common:../hindustani

concrete LangNep of Lang = 
  GrammarNep,
  LexiconNep
  ** {

flags startcat = Phr ; unlexer=unwords ; lexer=words ;

}
