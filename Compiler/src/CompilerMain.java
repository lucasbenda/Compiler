
import Tokenizer.MicroPascalScanner;
import Parser.Parser;
import Analyzer.Analyzer;
import Tokenizer.Token;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;

/**
 *
 * @author Lucas
 */
public class CompilerMain {

    public static void main(String[] args) throws IOException {
        File inputFile = new File("test/asdf.txt");
        File outputFile = new File("test/output.txt");
        String output = "test/output.txt";
        
        LinkedList<Token> list = new LinkedList<>();

        MicroPascalScanner sc = new MicroPascalScanner(inputFile);
        list = sc.getAllTokens();

        for (Token current : list) {
            //System.out.print("  >>  "+"Ln:   "+current.getLineNumber()+"\tCl: "+current.getColumnNumber()+"\t\tType:  "+current.getToken()+"\t\tLex:  "+current.getLexeme());
            System.out.printf("    %1$-14s     %2$-20s     " + current.getLexeme() + "\n", "[" + current.getLineNumber() + ", " + current.getColumnNumber() + "]", current.getToken());

        }
        
        Parser parser = new Parser(list);
        

    }

}
