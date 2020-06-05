// Generated from Gr.g4 by ANTLR 4.5.1

package fl;


import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

/**
 * This class provides an empty implementation of {@link GrListener},
 * which can be extended to create a listener which only needs to handle a subset
 * of the available methods.
 */
public class GrBaseListener implements GrListener {
    int i = 0;
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterProgram(GrParser.ProgramContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Target");
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitProgram(GrParser.ProgramContext ctx) {
        i--;
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterTarget(GrParser.TargetContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Target");

    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitTarget(GrParser.TargetContext ctx) {i--; }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterRelation(GrParser.RelationContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Relation");
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitRelation(GrParser.RelationContext ctx) {
	    i--;
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterHead(GrParser.HeadContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Head");

    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitHead(GrParser.HeadContext ctx) {
	    i--;
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterBody(GrParser.BodyContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Body");

    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitBody(GrParser.BodyContext ctx) {
	    i--;
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterArg(GrParser.ArgContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Argument");

    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitArg(GrParser.ArgContext ctx) {
	    i--;
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterAtom(GrParser.AtomContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Atom");

    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitAtom(GrParser.AtomContext ctx) {
	    i--;
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterArgs(GrParser.ArgsContext ctx) {
	    i++;
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println("Args");

    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitArgs(GrParser.ArgsContext ctx) {
	    i--;
    }

	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void enterEveryRule(ParserRuleContext ctx) {


    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void exitEveryRule(ParserRuleContext ctx) { }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */
	@Override public void visitTerminal(TerminalNode node) {
        for (int j = 0; j < i; j++) System.out.print(" ");
        System.out.println(node);
    }
	/**
	 * {@inheritDoc}
	 *
	 * <p>The default implementation does nothing.</p>
	 */




	@Override public void visitErrorNode(ErrorNode node) {

    }
}