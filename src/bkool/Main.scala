

/**
 * @author nhphung
 */

package bkool

import java.io.{PrintWriter,File}
import java.util.concurrent.{Executors,TimeUnit,TimeoutException}
import org.antlr.v4.runtime.ANTLRFileStream


trait Timed {
  def timeoutAfter(timeout: Long)(codeToTest: => Unit): Unit = {
    val executor = Executors.newSingleThreadExecutor
    val future = executor.submit(new Runnable {
      def run = codeToTest
    })

    try {
      future.get(timeout, TimeUnit.MILLISECONDS)
    }
    finally {
      executor.shutdown()
    }
  }
}

object Main extends Timed{

  def main(args: Array[String]): Unit = {
    if (args.length == 5) {
    val start = java.lang.Integer.parseInt(args(1)) //testcase bat dau, o day la 1
    val end = java.lang.Integer.parseInt(args(2)) //testcase ket thuc, o day la 100
    val indir = args(3) //thu muc chua testcase, o day la $TEST
    val outdir = args(4)  //thu muc chua file ket qua khi chay cho moi sinh vien, o day la $DEST
    //o phase 2 chi chay parser chu khong thuc hien viec check nen co the bo argument nay
    //val opt = args(4) //tuy chon che do chay
    val option = args(0).drop(1)
    val sepa = "//" // dung cho linux
      //chay tat ca testcase mot luc
    for (i <- start to end) {
      //khong in ra phan nay
      println("Test "+i)
      //file input tuong ung voi 1 testcase
      //val inputFile = indir+"\\"+i+".txt"
      
      val (source,dest,altdest) = option match {
        case "testrecogniser" => (new ANTLRFileStream(s"$indir$sepa$i.txt"),new PrintWriter(new File(s"$outdir$sepa$i.txt")),None) //dung trong linux
        case "testlexer" =>  (new ANTLRFileStream(s"$indir$sepa$i.txt"),new PrintWriter(new File(s"$outdir$sepa$i.txt")),None) //dung trong linux
        case "testassignment1" =>  (new ANTLRFileStream(s"$indir$sepa$i.txt"),new PrintWriter(new File(s"$outdir${sepa}lexer$sepa$i.txt")),
                                    Some(new PrintWriter(new File(s"$outdir${sepa}recognizer$sepa$i.txt"))))
        case _ => throw new ClassCastException
      }
      //val outputFile = outdir+"//"+i+".txt" //dung trong linux
      //val source = Source.fromFile(inputFile)
       //dung trong linux, dest la cac file tam 1.txt, 2.txt
      //val lines = source.getLines
      //val input = if (!lines.isEmpty) lines.reduceLeft[String](_ + '\n' + _) else ""
      //val source = new ANTLRFileStream(inputFile)
      //val dest = new PrintWriter(new File(outputFile ))
      
      /*val parser:Any = option match {
        case "testrecogniser" => TestLexer.test(inputFile,)
        case "testlexer" => new BKOOLLexer
        
      }*/
      
      try 
      {
        timeoutAfter(1000)  //thoi gian chay la 0.5s
        {
            option match {
              
              case "testrecogniser" => bkool.parser.TestParser.test(source,dest)
              case "testlexer" => parser.TestLexer.test(source,dest)
              case "testassignment1" => {
                parser.TestLexer.test(source,dest)
                bkool.parser.TestParser.test(source,altdest.get)
              }
              case _ => throw new ClassCastException
          }
        }
      } 
      catch 
      {
        case te: TimeoutException => dest.println("Test runs timeout")
        case e : Exception => dest.println(e)
      } 
      finally 
      {
        //source.close()
        dest.close()
        altdest match {
          case Some(d) => d.close()
          case None => {}
        }
      }
      if(i == end) {
        System.exit(1)    
      }
    }
  } else  println("Usage: scala Main -option <start> <end> <in directory>  <out directory>")
  }
}