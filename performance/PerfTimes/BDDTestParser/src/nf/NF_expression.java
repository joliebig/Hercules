package nf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import main.BDDUtils;
import main.BddManager;
import main.Pair;
import net.sf.javabdd.BDD;

public class NF_expression {

	private Map<String, NF_variable> variables = new HashMap<String, NF_variable>();
	private Collection<NF_clause> clauses = new HashSet<NF_clause>();
		
	public void addClause(NF_clause cl) {
		clauses.add(cl);
	}
	public Collection<NF_clause> getClauses() {
		return clauses;
	}
	public NF_variable getVariable(String name) {
		NF_variable var = variables.get(name);
		if (var==null) {
			var = new NF_variable(name);
			variables.put(name, var);
		}
		return var;
	}
	public Collection<NF_variable> getVariables() {
		return variables.values();
	}
	public void setupVariableOrdering_FORCE() {
		long startTime = System.currentTimeMillis();
		Map<NF_variable, Integer> order = new HashMap<>();
		Map<NF_variable, Integer> oldOrder = new HashMap<>();
		//initial ordering
		{
			int i = 0;
			for (NF_variable var: getVariables()) {
				order.put(var, i++);
			}
		}
		int initialSpan= span(order, this);
		int oldSpan = Integer.MAX_VALUE;
		int span = initialSpan;
		// improve
		int iterations = 0;
		int maxIterations = 50;
		while (span < oldSpan && iterations <= maxIterations) {
			oldSpan = span;
			oldOrder = order;
			order = new HashMap<>();
			iterations++;
			Map<NF_variable, List<Double>> newGravityCenters = new HashMap<>(this.getVariables().size());
			for (NF_variable var : this.getVariables())
				newGravityCenters.put(var, new ArrayList<Double>(3));
			for (NF_clause clause : this.getClauses()) {
				double clauseGravityCenter = 0;
				for (NF_variable var : clause.getVariables())
					clauseGravityCenter += oldOrder.get(var);
				clauseGravityCenter = clauseGravityCenter/clause.getLiterals().size();
				for (NF_variable var : clause.getVariables())
					newGravityCenters.get(var).add(clauseGravityCenter);
			}
			List<Pair<NF_variable, Double>> newTentativePositions = new ArrayList<Pair<NF_variable, Double>>(this.getVariables().size());
			for (NF_variable var : this.getVariables()) {
				double tentativePos = avg(newGravityCenters.get(var));
				newTentativePositions.add(new Pair<NF_variable, Double>(var,tentativePos));
			}
			// TODO: use heap?
			Collections.sort(newTentativePositions, new Comparator<Pair<NF_variable, Double>>() {
				@Override
				public int compare(Pair<NF_variable, Double> o1,
						Pair<NF_variable, Double> o2) {
					return Double.compare(o1._2, o2._2);
				}
			});
			int newPosition = 0;
			for (Pair<NF_variable, Double> p : newTentativePositions) {
				order.put(p._1, newPosition++);
			}
			span = span(order, this);
		}
		// new ordering (order/span) is worse; use old ordering (oldOrder/oldSpan)
		NF_variable[] arr = new NF_variable[this.getVariables().size()];
		for (NF_variable var : this.getVariables())
			arr[oldOrder.get(var)] = var;
		for (NF_variable var : arr)
			BddManager.getVariableRegion(var.getName());
		long totalTime = System.currentTimeMillis()-startTime;
		System.out.println("FORCE variable ordering: " +iterations+" iterations, " + "Span:initial:"+initialSpan +" new:"+oldSpan + " "+ totalTime+"ms");
	}
	private static double avg(List<Double> list) {
		double sum = 0.0;
		for (double x : list) {
			sum += x;
		}
		return sum/list.size();
	}
	private static int span(Map<NF_variable,Integer> order, NF_expression cnf) {
		int totalSpan = 0;
		for ( NF_clause c : cnf.getClauses()) {
			int minPos = Integer.MAX_VALUE;
			int maxPos = 0;
			for (NF_literal lit : c.getLiterals()) {
				int pos = order.get(lit.getVariable());
				if (minPos > pos) minPos=pos;
				if (maxPos < pos) maxPos=pos;
			}
			totalSpan += (maxPos-minPos);
		}
		return totalSpan;
	}
	public BDD loadCNFasBDD() {
		long start = System.currentTimeMillis();
		BDD ret = BddManager.trueFormula.id();
		int clauseId = 0;
		//System.out.println("total clauses: " + this.getClauses().size());
		for (NF_clause clause : this.getClauses()) {
			clauseId++;
			//if (clauseId%100==0) System.out.println("clause " + clauseId);
			BDD clauseBDD = BddManager.falseFormula.id();
			for ( NF_literal lit : clause.getLiterals()) {
				BDD var = BddManager.getVariableRegion(lit.getVariable().getName());
				if (lit.isPositive())
					clauseBDD = clauseBDD.orWith(var.id());
				else
					clauseBDD = clauseBDD.orWith(var.not());
			}
			ret = ret.andWith(clauseBDD);
		}
		System.out.println("total clauses: " + this.getClauses().size() + "; parsing time: " + (System.currentTimeMillis()-start) + "ms");
		return ret;
	}
	public BDD loadDNFasBDD() {
		long start = System.currentTimeMillis();
		int clauseId = 0;
		//System.out.println("total clauses: " + this.getClauses().size());
		ArrayList<BDD> clauses = new ArrayList<>(this.getClauses().size());
		for (NF_clause clause : this.getClauses()) {
			clauseId++;
			//if (clauseId%100==0) System.out.println("clause " + clauseId);
			BDD clauseBDD = BddManager.trueFormula.id();
			for ( NF_literal lit : clause.getLiterals()) {
				BDD var = BddManager.getVariableRegion(lit.getVariable().getName());
				if (lit.isPositive())
					clauseBDD = clauseBDD.andWith(var.id());
				else
					clauseBDD = clauseBDD.andWith(var.not());
			}
			clauses.add(clauseBDD);
		}
		BDD ret = BDDUtils.efficientOrWithAll(clauses);
		System.out.println("total clauses: " + this.getClauses().size() + "; parsing time: " + (System.currentTimeMillis()-start) + "ms");
		return ret;
	}
	@Override
	public String toString() {
		return clauses.toString();
	}
}
