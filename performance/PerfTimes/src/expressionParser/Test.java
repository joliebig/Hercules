package expressionParser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import main.BddManager;
import net.sf.javabdd.BDD;

public class Test {		
	/*public static void main(String[] args) throws Exception {
		File featureModelFile = new File("Formula.txt");
		FeatureModelParser p = new FeatureModelParser(new FeatureModelScanner(new FileReader(featureModelFile)));
		
		Constraint modelConstraints;
		// parsing happens here
		modelConstraints = (Constraint) p.parse().value;

		// evaluation
		Model m = new CPModel();
		m.addConstraint(modelConstraints);
		Solver solver;
		solver = new CPSolver();
		solver.read(m);
		solver.solveAll();
		if (! solver.isFeasible()) {
			System.out.println("infeasible");
		} else {
			System.out.println(solver.getSolutionCount() + " solutions");
		}*/
	public static void main(String[] args) throws Exception {
		/*
		FeatureModelParser p = new FeatureModelParser(new FeatureModelScanner(new StringReader(
				"A & ! B | C")));
		System.out.println(BddManager.BDDToString((BDD)p.parse().value, false, false));
		*/
		
		testDefined();
	}
	public static void testOperatorPrecedence() throws Exception {
		
		BDD a = BddManager.getVariableRegion("A");
		BDD b = BddManager.getVariableRegion("B");
		BDD c = BddManager.getVariableRegion("C");
		// test operator precedence
		FeatureModelParser p = new FeatureModelParser(new FeatureModelScanner(new StringReader(
				"A and not B or C")));
		BDD model = (BDD) p.parse().value;
		if (!model.equals(
				(a.and(b.not()).or(c))
				)) {
			System.out.println("Operator precedence is wrong (1)");
		}
		p = new FeatureModelParser(new FeatureModelScanner(new StringReader(
				"A or not B and C")));
		model = (BDD) p.parse().value;
		System.out.println(BddManager.BDDToString(model, false, false));
		if (!model.equals(
				a.or(b.not().and(c))
			)) {
				System.out.println("Operator precedence is wrong (2)");
			}
	}
	
	public static void testDefined() throws Exception {
		
		// test operator precedence
		FeatureModelParser p = new FeatureModelParser(new FeatureModelScanner(new StringReader(
				"defined(FOO)")));
		BDD model = (BDD) p.parse().value;
		if (!model.equals(
				(BddManager.getVariableRegion("FOO"))
				)) {
			System.out.println("Parsing Failure");
		} else {
			System.out.println("test ok");
		}
	}
	
	@SuppressWarnings("unused")
	private static List<String> scanFeatureNames(File featureModelFile) throws IOException {
		try (BufferedReader br = new BufferedReader(new FileReader(featureModelFile))) {
			int read;
			String currentVar ="";
			HashSet<String> collection= new HashSet<String>();
			while ((read = br.read()) != -1) {
				char r = (char) read;
				if (currentVar.isEmpty()) {
					if (Character.isJavaIdentifierStart(r)) {
						currentVar = "" + r;
					} else {
						//skip this char
					}
				} else {
					if (Character.isJavaIdentifierPart(r)) {
						currentVar = currentVar + r;
					} else {
						//terminate var
						if (!currentVar.equals("and") && !currentVar.equals("or"))
							collection.add(currentVar);
						currentVar="";
					}
				}
			}
			
			if (!currentVar.isEmpty()) {
				collection.add(currentVar);
			}
			return new ArrayList<String>(collection);
		}
	}
}

