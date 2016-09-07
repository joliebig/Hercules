package nf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class NF_clause_Set implements NF_clause {
	Set<NF_literal> literals = new HashSet<NF_literal>();
	
	public Set<NF_literal> getLiterals() {
		return literals;
	}
	public int getSize() {
		return literals.size();
	}
	public void addLiteral(NF_literal lit) {
		literals.add(lit);
	}
	public void addLiteral(NF_variable var, boolean positive) {
		literals.add(new NF_literal(var, positive));
	}
	@Override
	public Collection<NF_variable> getVariables() {
		ArrayList<NF_variable> ret = new ArrayList<NF_variable>(literals.size());
		for (NF_literal lit : literals)
			ret.add(lit.getVariable());
		return ret;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("(");
		for (NF_literal lit : literals) {
			if (lit.isPositive())
				sb.append(lit.getVariable().getName() + " ");
			else
				sb.append("~" + lit.getVariable().getName() + " ");
		}
		sb.append(")");
		return sb.toString();
	}
	@Override
	public int hashCode() {
		return literals.hashCode();
	}
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof NF_clause_Set))
			return false;
		else
			return ((NF_clause_Set)obj).literals.equals(literals);
	}
	
}
