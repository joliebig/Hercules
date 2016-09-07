package main;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;
import net.sf.javabdd.BDDFactory.ReorderMethod;


/**
 * This class manages the BDDs.
 * It holds a mapping between BDDs and variable names.
 * Important methods are
 *  getting/creating a BDD (associated to a variable name)
 *  getting a String representation of a BDD (with the variable names)
 * @author rhein
 */
public class BddManager implements Cloneable {
	  //private static final String BDD_PACKAGE = "buddy";
	private static final String BDD_PACKAGE = "JDD"; // default package, Java implementation
//	private static final String BDD_PACKAGE = "buddy";
//	private static final int bddLibInitialNodeSize = 10000000, bddLibCacheSize = 1000000;
	private static final int bddLibInitialNodeSize = 30000000, bddLibCacheSize = bddLibInitialNodeSize/10;// needs -Xmx4G
	//private static final int bddLibInitialNodeSize = 400000000, bddLibCacheSize = bddLibInitialNodeSize/10;// needs -Xmx24G
	public static BDDFactory factory = BDDFactory.init(BDD_PACKAGE, bddLibInitialNodeSize, bddLibCacheSize); 
	  public static BDD trueFormula = factory.one();
	  public static BDD falseFormula = factory.zero();
	public static Map<String, BDD> varToBDD = new HashMap<String, BDD>();
	public static Map<BDD, String> BDDToVar = new HashMap<BDD, String>();
	
	private static int nextvar = 0;
	private static int varcount = 100;

	//private constructor
	private BddManager() {
	}
	
	private static BDD createNewVar() {
	  if (nextvar >= varcount) {
	    varcount *= 1.5;
	    factory.setVarNum(varcount);
	  } else if (factory.varNum() != varcount) { 
		  factory.setVarNum(varcount);
	  }
	  BDD ret = factory.ithVar(nextvar++);
	  return ret;
	}
	
	public static void reorder(ReorderMethod strategy) {
		factory.reorder(strategy);
	}

	public static BDD getVariableRegion(String pVarName) {
		BDD ret = varToBDD.get(pVarName);
		if (ret == null) {
			ret = createNewVar();
			varToBDD.put(pVarName, ret);
			BDDToVar.put(ret, pVarName);
			//System.out.println("Created BDD variable " +(nextvar-1) + " for " + pVarName);
		}
		return ret;
	}
	
	/**
	 * Clears the static fields of this class (e.g. BDD-to-variable mappings).
	 * Useful for tests where the (static) instance of this class will be reused across multiple verification runs.
	 */
	public static void resetStatics() {
		factory = BDDFactory.init(BDD_PACKAGE, bddLibInitialNodeSize, bddLibCacheSize);
		trueFormula = factory.one();
		falseFormula = factory.zero();
		varToBDD.clear();
		BDDToVar.clear();
		
		factory.clearVarBlocks();
		factory.clearAllDomains();
		nextvar = 0;
		varcount = 100;  
		factory.setVarNum(varcount);
	}
	enum OutermostOp {
		AND, OR, NONE
	}
	/**
	 * Returns a String representation of the parameter.
	 * It is important that every predicate in the region is contained in the regionMap.
	 * @param r
	 * @return
	 */
	private static Pair<String, OutermostOp> bddToStringPriv(BDD r, boolean abbreviate, boolean optimizePartFormulas) {
		if (BDDToVar.containsValue(r)) {
			return new Pair<String, OutermostOp>(BDDToVar.get(r), OutermostOp.NONE);
		} else if (isFalse(r))
			return new Pair<String, OutermostOp>("FALSE", OutermostOp.NONE);
		else if (isTrue(r))
			return new Pair<String, OutermostOp>("TRUE", OutermostOp.NONE);
		else {
			Triple<BDD, BDD, BDD> triple = getIfThenElse(r);
			
			String predName = BDDToVar.get(triple.getFirst());
			if (abbreviate && predName.contains(".") && ! predName.endsWith(".")) {
				predName = predName.substring(predName.lastIndexOf(".")+1);
				if (predName.startsWith("__SELECTED_FEATURE_")) {
					predName = predName.substring("__SELECTED_FEATURE_".length());
				}
			}
			//if (isTrue(triple.getSecond()) && isTrue(triple.getThird())) {} else // cannot happen
			//} else if (isFalse(triple.getSecond()) && isFalse(triple.getThird())) {} else // cannot happen
			if (isTrue(triple.getSecond()) && isFalse(triple.getThird())) {
				return new Pair<String, OutermostOp>(predName, OutermostOp.NONE);
			} else if (isFalse(triple.getSecond()) && isTrue(triple.getThird())) {
				return new Pair<String, OutermostOp>("!" + predName, OutermostOp.NONE);
			} else if (isTrue(triple.getSecond())) {
				Pair<String, OutermostOp> sub = BDDToStringWithOpInfo(triple.getThird(), abbreviate, optimizePartFormulas);
				String subS = (sub._2!=OutermostOp.NONE?"(":"") + sub._1 + (sub._2!=OutermostOp.NONE?")":"");
				return new Pair<String, OutermostOp>(predName + " || " + subS, OutermostOp.OR);
			} else if (isFalse(triple.getSecond())) {
				Pair<String, OutermostOp> sub = BDDToStringWithOpInfo(triple.getThird(), abbreviate, optimizePartFormulas);
				String subS = (sub._2==OutermostOp.OR?"(":"") + sub._1 + (sub._2==OutermostOp.OR?")":"");
				return new Pair<String, OutermostOp>("!" + predName + " && " + subS, OutermostOp.AND);
			} else if (isTrue(triple.getThird())) {
				Pair<String, OutermostOp> sub = BDDToStringWithOpInfo(triple.getSecond(), abbreviate, optimizePartFormulas);
				String subS = (sub._2!=OutermostOp.NONE?"(":"") + sub._1 + (sub._2!=OutermostOp.NONE?")":"");
				return new Pair<String, OutermostOp>("!" + predName + " || " + subS, OutermostOp.OR);
			} else if (isFalse(triple.getThird())) {
				Pair<String, OutermostOp> sub = BDDToStringWithOpInfo(triple.getSecond(), abbreviate, optimizePartFormulas);
				String subS = (sub._2==OutermostOp.OR?"(":"")  + sub._1 + (sub._2==OutermostOp.OR?")":"");
				return new Pair<String, OutermostOp>(predName + " && " + subS, OutermostOp.AND);
			} else {
				Pair<String, OutermostOp> sub1 = BDDToStringWithOpInfo(triple.getSecond(), abbreviate, optimizePartFormulas);
				String subS1 = (sub1._2==OutermostOp.OR?"(":"")  + sub1._1 + (sub1._2==OutermostOp.OR?")":"");
				Pair<String, OutermostOp> sub2 = BDDToStringWithOpInfo(triple.getThird(), abbreviate, optimizePartFormulas);
				String subS2 = (sub2._2==OutermostOp.OR?"(":"")  + sub2._1 + (sub2._2==OutermostOp.OR?")":"");
				return new Pair<String, OutermostOp>("(" + predName + " && " + subS1 + ")"
						+ " || " + 
						"(!" + predName + " && " + subS2 + ")", OutermostOp.OR);
			}
		}
	}
	
	/**
	 * Determines which variables must be TRUE (FALSE) in this formula and sets them to the beginning of the returned String.
	 * This is much more readable than the default version, but needs more time because existence quantification is used.
	 */
	static Pair<String, OutermostOp> bddToOptimizedString(BDD r, boolean abbreviate) {
		Set<BDD> mandatoryTrue = new HashSet<BDD>();
		Set<BDD> mandatoryFalse = new HashSet<BDD>();
		
		for (BDD varBdd: varToBDD.values()) {
			if (r.and(varBdd).isZero())
				mandatoryFalse.add(varBdd);
			else if (r.and(varBdd.not()).isZero())
				mandatoryTrue.add(varBdd);
		}
		String prefix = "";
		for (BDD x : mandatoryTrue) {
			r = r.exist(x);
			String predName = BDDToVar.get(x);
			if (abbreviate && predName.contains(".") && ! predName.endsWith(".")) {
				predName = predName.substring(predName.lastIndexOf(".")+1);
				if (predName.startsWith("__SELECTED_FEATURE_")) {
					predName = predName.substring("__SELECTED_FEATURE_".length());
				}
			}
			prefix = prefix + predName + " & ";
		}
		for (BDD x : mandatoryFalse) {
			r = r.exist(x);
			String predName = BDDToVar.get(x);
			if (abbreviate && predName.contains(".") && ! predName.endsWith(".")) {
				predName = predName.substring(predName.lastIndexOf(".")+1);
				if (predName.startsWith("__SELECTED_FEATURE_")) {
					predName = predName.substring("__SELECTED_FEATURE_".length());
				}
			}
			prefix = prefix + "!" + predName + " & ";
		}
		if (prefix.trim().isEmpty()) {
			return bddToStringPriv(r, abbreviate, true);
		} else {
			Pair<String, OutermostOp> corePair = bddToStringPriv(r, abbreviate, true);
			String coreFormula = (corePair._2==OutermostOp.OR?"(":"")  + corePair._1 + (corePair._2==OutermostOp.OR?")":"");
			return new Pair<String,OutermostOp> (prefix + coreFormula, OutermostOp.AND);
		}
	}
	public static String BDDToString (BDD r, boolean abbreviate, boolean optimize) {
		return BDDToStringWithOpInfo(r, abbreviate, optimize)._1;
	}
	public static Pair<String,OutermostOp> BDDToStringWithOpInfo (BDD r, boolean abbreviate, boolean optimize) {
		if (!optimize)
			return bddToStringPriv(r, abbreviate, optimize);
		else 
			return bddToOptimizedString(r, abbreviate);
	}
	
	public static Triple<BDD, BDD, BDD> getIfThenElse(BDD pBDD) {
		BDD predicate = factory.ithVar(pBDD.var());
		BDD fThen = pBDD.high();
		BDD fElse = pBDD.low();
		return new Triple<BDD, BDD, BDD>(predicate, fThen, fElse);
	}
	
	/**
	 * Returns a short representation of the given BDD.
	 * The package names of the variable's classes are omitted.
	 */
	public static String toAbbrevString(BDD bdd) {
		return bddToStringPriv(bdd, true, false)._1;
	}
	public static String toAbbrevString(BDD bdd, boolean optimize) {
		if (optimize)
			return bddToOptimizedString(bdd, true)._1;
		else
			return bddToStringPriv(bdd, true, false)._1;
	}
	public static boolean isFalse(BDD bdd) {
		return bdd.isZero();
	}
	
	public static boolean isTrue(BDD other) {
		return other.isOne();
	}

	/** Count how many "concrete" states are represented by this bdd.
	 */
	public static long reprStates(BDD bdd) {
		// we have seen -nextvar- tracked variables
		// how many are used in this bdd?
		long usedVars = countNonzeroEntries(bdd.varProfile());
		// how many solutions are allowed by this bdd?
		long solutions = bdd.allsat().size();
		return (long) (solutions * Math.pow(2, nextvar-usedVars));
	}
	private static long countNonzeroEntries(int[] array) {
		long ret = 0;
		for (int entry : array) if (entry != 0) ret++;
		return ret;
	}
	
	public static void BDDtoDotFile(BDD bdd, File file, String title) throws IOException {
		boolean replaceVarNames=true;
		PrintStream stdOut = System.out;
		System.setOut(new PrintStream(file));
		bdd.printDot();
		System.setOut(stdOut);
		
		String dotContents = "";
		try(BufferedReader br = new BufferedReader(new FileReader(file))) {
			String line = "";
			while ((line=br.readLine()) != null) {
				dotContents += line +"\n";
			}
			dotContents = dotContents.replaceFirst("\n", "\nlabel=\""+title+"\";\n"+
					"labelloc=top;\n"+
					"labeljust=center;\n");
			if (replaceVarNames) {
				for (BDD key : BddManager.BDDToVar.keySet()) {
					dotContents = dotContents.replaceAll(
							"\\[label=\""+key.level()+"\"\\]", 
							"\\[label=\""+key.level()+":"+BddManager.BDDToVar.get(key)+"\"\\]");
				}
			}
		}
		try(FileWriter fw = new FileWriter(file)) {
			fw.write(dotContents);
		}
		//System.out.println(Main.arrayToString(bdd.varProfile()));
		
		
	}
	public static long getOpsInStringRep(BDD r) {
		if (BDDToVar.containsValue(r)) {
			return 0;
		} else if (isFalse(r))
			return 0;
		else if (isTrue(r))
			return 0;
		else {
			Triple<BDD, BDD, BDD> triple = getIfThenElse(r);
			if (isTrue(triple.getSecond()) && isFalse(triple.getThird())) {
				return 0;
			} else if (isFalse(triple.getSecond()) && isTrue(triple.getThird())) {
				return 0;
			} else if (isTrue(triple.getSecond())) {
				return 1+getOpsInStringRep(triple.getThird());
			} else if (isFalse(triple.getSecond())) {
				return 1+getOpsInStringRep(triple.getThird());
			} else if (isTrue(triple.getThird())) {
				return 1 + getOpsInStringRep(triple.getSecond());
			} else if (isFalse(triple.getThird())) {
				return 1 + getOpsInStringRep(triple.getSecond());
			} else {
				return 3 +getOpsInStringRep(triple.getSecond()) + 
						getOpsInStringRep(triple.getThird());
			}
		}
	}
}
