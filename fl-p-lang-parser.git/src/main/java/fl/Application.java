package fl;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import org.apache.commons.cli.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Application {

    public static void main(String[] args) throws ParseException, IOException {
        Options options = new Options();
        options.addOption("i", "input", true, "Path to input file");

        DefaultParser cmdParser = new DefaultParser();
        CommandLine ops = cmdParser.parse(options, args);

        if(ops.hasOption("i")){
            String i = ops.getOptionValue("i");
            byte[] bytes = Files.readAllBytes(Paths.get(i));
            String input = new String(bytes);
            ANTLRInputStream stream1 = new ANTLRInputStream(input);
            CommonTokenStream commonTokenStream = new CommonTokenStream(new GrLexer(stream1));
            GrParser parser = new GrParser(commonTokenStream);
            parser.setBuildParseTree(true);
            GrParser.ProgramContext tree = parser.program();
            GrBaseListener grBaseListener = new GrBaseListener();
            ParseTreeWalker.DEFAULT.walk(grBaseListener, tree);
        }else{
            System.out.println("Use -i <path_to_program> option");
        }

    }
}
