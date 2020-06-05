package fl;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;

import java.util.stream.Stream;

import static org.junit.Assert.*;

public class GrParserTest {

    private static SyntaxErrorListener parseProgram(String input){
        ANTLRInputStream stream1 = new ANTLRInputStream(input);
        CommonTokenStream commonTokenStream = new CommonTokenStream(new GrLexer(stream1));
        GrParser parser = new GrParser(commonTokenStream);
        SyntaxErrorListener errorListener = new SyntaxErrorListener();
        parser.addErrorListener(errorListener);
        parser.setBuildParseTree(true);
        parser.program();
        return errorListener;
    }

    @Test
    public void test1(){
        String input = "eval(St, var(X), U) :- elem(X, St, U).\n" +
                "eval(St, conj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), and(V, W, U).\n" +
                "eval(St, disj(X,Y), U) :- eval(St, X, V), eval(St, Y, W), or(V, W, U).\n" +
                "eval(St, not(X), U) :- eval(St, X, V), neg(U, V).\n" +
                "elem(zero, cons(H,T), H).\n" +
                "elem(succ(N), cons(H,T), V) :- elem(N, T, V).\n" +
                "nand(false, false, true).\n" +
                "nand(false, true, true).\n" +
                "nand(true, false, true).\n" +
                "nand(true, true, false).\n" +
                "neg(X, R) :- nand(X, X, R).\n" +
                "or(X, Y, R) :- nand(X, X, Xx), nand(Y, Y, Yy), nand(Xx, Yy, R).\n" +
                "and(X, Y, R) :- newqeeqwqeqand(X, Y, Xy), nand(Xy, Xy, R).\n" +
                "?- eval(St, conj(disj(X,Y),not(var(Z))), true).";


        assertFalse(parseProgram(input).isFailure());
    }

    @Test
    public void test2(){
        String input = "eval(St, var(X), U) :- elem(X, St, U). ?-";
        assertTrue(parseProgram(input).isFailure());
    }

    @Test
    public void test3(){
        String input = "eval(St, var(X), U) . ?- elem .";
        assertFalse(parseProgram(input).isFailure());
    }

    @Test
    public void test4(){
        String input = "eval(St, var(X), U) . ?- .";
        assertFalse(parseProgram(input).isFailure());
    }

    @Test
    public void test5(){
        String input = "?- .";
        assertFalse(parseProgram(input).isFailure());
    }

    @Test
    public void test6(){
        String input = "?- gr, tr .";
        assertFalse(parseProgram(input).isFailure());
    }
}