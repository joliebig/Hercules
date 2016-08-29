package main;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import main.BddManager;
import main.Pair;
import net.sf.javabdd.BDD;
import main.nf.NF_clause_Array;
import main.nf.NF_expression;
import main.nf.NF_variable;

public class DimacsParser {
	/**
	 * Data class for a CNF parsed from a dimacs file.
	 */
	public static class DimacsCNF {
		/** The cnf expression */
		NF_expression cnf;
		/** Set of variables that were introduced while transforming the expression to dimacs/CNF.
		 * These vars should not be user-visible. */
		Set<NF_variable> artificialVariables;
		DimacsCNF(NF_expression cnf, Set<NF_variable> artificialVariables) {
			this.cnf = cnf;
			this.artificialVariables = artificialVariables;
		}
		public NF_expression getCnf() {
			return cnf;
		}
		public Set<NF_variable> getArtificialVariables() {
			return artificialVariables;
		}
		
	}
	public static BDD parseDimacs(File dimacsFile) throws Exception {
		Pair<BDD,Set<BDD>> pair = getClauseGroupsAndArtifVars(dimacsFile);
		Set<BDD> artificialVariables = pair._2;
		BDD ret = pair._1;
		int id = 0;
		for (BDD entry : artificialVariables) {
			System.out.println("existArtificial var " + (id++) + " of " + artificialVariables.size());
			ret = ret.exist(entry);
		}
		return ret;
	}
	public static Pair<BDD, Set<BDD>> getClauseGroupsAndArtifVars(File dimacsFile) throws Exception {
		DimacsCNF dimacsCNF = parseDimacsCNF(dimacsFile);
		Set<NF_variable> artificialVariables = dimacsCNF.artificialVariables;
		NF_expression dimacsFM = dimacsCNF.cnf;
		
		dimacsFM.setupVariableOrdering_FORCE();
		
		BDD fm = dimacsFM.loadCNFasBDD(); //dimacs is CNF
		Set<BDD> artBDDs = new HashSet<BDD>(artificialVariables.size());
		for (NF_variable var : artificialVariables)
			artBDDs.add(BddManager.getVariableRegion(var.getName()));
		return new Pair<BDD,Set<BDD>>(fm, artBDDs);
	}
	
	public static DimacsCNF parseDimacsCNF(File dimacsFile) throws Exception {
		HashSet<NF_variable> artificialVariables = new HashSet<>();
		HashMap<Integer, NF_variable> varMap = new HashMap<>();
		//CNFFormula dimacsFM = new CNFFormula();
		NF_expression dimacsFM = new NF_expression();
		
		try (BufferedReader br = new BufferedReader(new FileReader(dimacsFile))) {
			String line = null;
			while ((line=br.readLine())!=null) {
				//System.out.println("parsing dimacs line " + lineID);
				String trimmedLine = line.trim();
				if(trimmedLine.startsWith("c ")) {
					// variable 
					// c 16$ _X1172_m
					String[] lineComp = trimmedLine.split(" ");
					//BooleanVariable var = new BooleanVariable(lineComp[2]);
					NF_variable var = dimacsFM.getVariable(lineComp[2]);
					assert var.getName().equals(lineComp[2]);
					int id;
					if (lineComp[1].endsWith("$")) {
						id = Integer.parseInt(lineComp[1].substring(0, lineComp[1].length()-1));
						artificialVariables.add(var);
					} else
						id = Integer.parseInt(lineComp[1]);
					varMap.put(id, var);
				} else if(trimmedLine.startsWith("p ")) {
					// ignore
				} else if (!trimmedLine.isEmpty()){
					// clause
					// -3752 -2622 3900 0
					//CNFClause clause = new CNFClause();
					assert (trimmedLine.endsWith(" 0"));
					String[] clauseComp = trimmedLine.split(" ");
					NF_clause_Array clause = new NF_clause_Array(clauseComp.length-1);
					for (int i = 0; i < clauseComp.length-1; i++) { // -1 because we are not interested in the zero at the end
						int varId = Integer.parseInt(clauseComp[i]);
						int absVarId = Math.abs(varId);
						NF_variable variable = varMap.get(absVarId);
						if (variable == null) {// must be an artificial variable that was not listed in the comments section ("c ...")
							variable = dimacsFM.getVariable("unnamedDimacsVar"+varId);
							artificialVariables.add(variable);
							varMap.put(absVarId, variable);
						}
						if (varId > 0)
							clause.addLiteral(i, variable, true);
						else 
							clause.addLiteral(i, variable, false);
					}
					dimacsFM.addClause(clause);
				}
			}
		}
		return new DimacsCNF(dimacsFM, artificialVariables);
	}
}
