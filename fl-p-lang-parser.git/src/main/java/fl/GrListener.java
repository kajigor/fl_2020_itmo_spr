// Generated from Gr.g4 by ANTLR 4.5.1

package fl;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link GrParser}.
 */
public interface GrListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link GrParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(GrParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(GrParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#target}.
	 * @param ctx the parse tree
	 */
	void enterTarget(GrParser.TargetContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#target}.
	 * @param ctx the parse tree
	 */
	void exitTarget(GrParser.TargetContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#relation}.
	 * @param ctx the parse tree
	 */
	void enterRelation(GrParser.RelationContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#relation}.
	 * @param ctx the parse tree
	 */
	void exitRelation(GrParser.RelationContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#head}.
	 * @param ctx the parse tree
	 */
	void enterHead(GrParser.HeadContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#head}.
	 * @param ctx the parse tree
	 */
	void exitHead(GrParser.HeadContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#body}.
	 * @param ctx the parse tree
	 */
	void enterBody(GrParser.BodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#body}.
	 * @param ctx the parse tree
	 */
	void exitBody(GrParser.BodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#arg}.
	 * @param ctx the parse tree
	 */
	void enterArg(GrParser.ArgContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#arg}.
	 * @param ctx the parse tree
	 */
	void exitArg(GrParser.ArgContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#atom}.
	 * @param ctx the parse tree
	 */
	void enterAtom(GrParser.AtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#atom}.
	 * @param ctx the parse tree
	 */
	void exitAtom(GrParser.AtomContext ctx);
	/**
	 * Enter a parse tree produced by {@link GrParser#args}.
	 * @param ctx the parse tree
	 */
	void enterArgs(GrParser.ArgsContext ctx);
	/**
	 * Exit a parse tree produced by {@link GrParser#args}.
	 * @param ctx the parse tree
	 */
	void exitArgs(GrParser.ArgsContext ctx);
}