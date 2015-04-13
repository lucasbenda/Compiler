package Tokenizer;

import java.io.File;
import java.util.LinkedList;

/**
 *
 * @author Lucas
 */
public class ScannerMain {

    public static void main(String[] args) {
        File inputFile = new File("test/asdf.txt");
        LinkedList<Token> list = new LinkedList<>();

        MicroPascalScanner sc = new MicroPascalScanner(inputFile);
        list = sc.getAllTokens();
        
        for (Token current : list) {
            //System.out.print("  >>  "+"Ln:   "+current.getLineNumber()+"\tCl: "+current.getColumnNumber()+"\t\tType:  "+current.getToken()+"\t\tLex:  "+current.getLexeme());
            System.out.printf("    %1$-14s     %2$-20s     " + current.getLexeme() + "\n", "[" + current.getLineNumber() + ", " + current.getColumnNumber() + "]", current.getToken());

        }
        
        
        
    }

}
