package expressionParser;
import java_cup.runtime.Symbol;

@SuppressWarnings(value = { "all" })
%%

%cup
%class FeatureModelScanner
%implements FeatureModelSym
%line
%column

%{
  private Symbol symbol(String name, int sym, String val) {
    return new Symbol(sym, yyline, yycolumn, val);
  }
  private Symbol symbol(String name, int type) {
    return new Symbol(type, yyline, yycolumn);
  }
  
  private void error(String message) {
    throw new RuntimeException(message + " near " + yyline +":" + yycolumn);
  }
%}
%eofval{
    return symbol("EOF", FeatureModelSym.EOF);
%eofval}

LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n] | .
WhiteSpace     = {LineTerminator} | [ \t\f]

/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}

TraditionalComment   = "/*" [^*] ~"*/" | "/*" "*"+ "/"
EndOfLineComment     = "//" {InputCharacter}* {LineTerminator}

Identifier = [:jletter:] [:jletterdigit:]*


%%

/* keywords */
<YYINITIAL> "!"                { return symbol("not", FeatureModelSym.NOT); }
<YYINITIAL> "&"                { return symbol("and", FeatureModelSym.AND); }
<YYINITIAL> "|"                 { return symbol("or", FeatureModelSym.OR); }

/* I know that && is lazy, but for the sake of simplicity its the same here. */
<YYINITIAL> "&&"                { return symbol("and", FeatureModelSym.AND); }
<YYINITIAL> "||"                 { return symbol("or", FeatureModelSym.OR); }

<YYINITIAL> "not"                { return symbol("not", FeatureModelSym.NOT); }
<YYINITIAL> "and"                { return symbol("and", FeatureModelSym.AND); }
<YYINITIAL> "or"                 { return symbol("or", FeatureModelSym.OR); }


<YYINITIAL> "oneOf"                { return symbol("oneOf", FeatureModelSym.ONE_OF); }
<YYINITIAL> "noneOf"                { return symbol("noneOf", FeatureModelSym.NONE_OF); }
<YYINITIAL> "someOf"                 { return symbol("someOf", FeatureModelSym.SOME_OF); }
<YYINITIAL> "oneOfOrNone"                 { return symbol("oneOfOrNone", FeatureModelSym.ONE_OF_OR_NONE); }

<YYINITIAL> "def"                 { return symbol("defined", FeatureModelSym.DEFINED); }
<YYINITIAL> "definedEx"                 { return symbol("defined", FeatureModelSym.DEFINED); }
<YYINITIAL> "defined"                 { return symbol("defined", FeatureModelSym.DEFINED); }

/* true/false */
<YYINITIAL> "TRUE"                           { return symbol("TRUE", FeatureModelSym.TRUE); }
<YYINITIAL> "FALSE"                           { return symbol("FALSE", FeatureModelSym.FALSE); }
<YYINITIAL> "True"                           { return symbol("TRUE", FeatureModelSym.TRUE); }
<YYINITIAL> "False"                           { return symbol("FALSE", FeatureModelSym.FALSE); }

<YYINITIAL> {
  /* identifiers */ 
  {Identifier}                   { return symbol("ID", FeatureModelSym.IDENTIFIER, yytext()); }
 
  /* literals */

  /* operators */
  "("                           { return symbol("(", FeatureModelSym.OBRACKETS); }
  ")"                           { return symbol(")", FeatureModelSym.CBRACKETS); }
  
  ","                           { return symbol("(", FeatureModelSym.COMMA); }


  /* comments */
  {Comment}                      { /* ignore */ }
 
  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }
}

<<EOF>> {return symbol("EOF", FeatureModelSym.EOF); }
/* error fallback */
.|\n                             { error("Illegal character <"+yytext()+">"); }
