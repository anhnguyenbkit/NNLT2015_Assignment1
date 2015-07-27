

/**
 * @author nhphung
 */

package bkool.parser
import org.antlr.v4.runtime.Token
import java.io.{PrintWriter,File}
import org.antlr.v4.runtime.ANTLRFileStream
import bkool.parser.ProcessError

object TestLexer extends ProcessError {
  
  def main(args:Array[String]):Unit = {
      val file = if (args.length > 0) args(0) else "test.txt";
      val infile = new ANTLRFileStream(file)
      val out = if (args.length > 1) args(1) else "output.txt" 
      val outFile = new PrintWriter(new File(out));
      test(infile,outFile)
      outFile.close()
  }
  def test(infile:ANTLRFileStream,outfile:PrintWriter) = {
      val lexer = new BKOOLLexer(infile);
      val _listener = createRecoverErrorListener(outfile);
      lexer.removeErrorListeners();
      lexer.addErrorListener(_listener);
      printLexeme(lexer,outfile)

  }
  def printAtt(lexer:BKOOLLexer,dev:PrintWriter,prn:Token=>String):Unit = {
    
      val tok = lexer.nextToken()
      if (tok.getType() != Token.EOF) {
        dev.println(prn(tok))
        printAtt(lexer,dev,prn)
      } else dev.print(prn(tok))
    
    
  }
  
  def printLexeme(lexer:BKOOLLexer,dev:PrintWriter) = printAtt(lexer,dev,_.getText())
  
  def printToken(lexer:BKOOLLexer,dev:PrintWriter) = printAtt(lexer,dev,x => BKOOLLexer.ruleNames(x.getType()-1))
  
  def printAll(lexer:BKOOLLexer,dev:PrintWriter) = printAtt(lexer,dev,x => x.getText() +"\t"+ BKOOLLexer.ruleNames(x.getType()-1))
  

}