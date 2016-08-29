package main.nf;

import java.util.Arrays;
import java.util.Collection;

public class NF_clause_Array implements NF_clause {
	NF_variable[] variables;
	boolean[] values;
	
	public NF_clause_Array(int size) {
		variables=new NF_variable[size];
		values=new boolean[size];
	}
	
	public int getSize() {
		return variables.length;
	}
	public Collection<NF_literal> getLiterals() {
		NF_literal[] ret = new NF_literal[getSize()];
		for (int i = 0; i < ret.length; i++)
			ret[i] = new NF_literal(variables[i], values[i]);
		return Arrays.asList(ret);
	}
	public void setLiteral(int pos, NF_literal lit) {
		variables[pos] = lit.getVariable();
		values[pos] = lit.isPositive();
	}
	public void addLiteral(int pos, NF_variable var, boolean positive) {
		variables[pos] = var;
		values[pos] = positive;
	}

	@Override
	public Collection<NF_variable> getVariables() {
		return Arrays.asList(variables);
	}
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("(");
		for (int i = 0; i < variables.length; i++) {
			
			if (values[i])
				sb.append(variables[i].getName() + " ");
			else
				sb.append("~" + variables[i].getName() + " ");
		}
		sb.append(")");
		return sb.toString();
	}
	public boolean equals(Object obj) {
		if (!(obj instanceof NF_clause_Array))
			return false;
		if (obj == this)
			return true;
		NF_clause_Array rhs = (NF_clause_Array) obj;
		if (rhs.variables.length != variables.length)
			return false;
		for (int i = 0; i < variables.length; i++) {
			boolean foundVar = false;
			for (int j = 0; !foundVar && j < rhs.variables.length; j++) {
				if (variables[i].equals(rhs.variables[j])) {
					foundVar = true;
					if (values[i] != rhs.values[j]) {
						return false;
					}
				}
			}
			if (!foundVar)
				return false;
		}
		return true;
	}

	@Override
	public int hashCode() {
		int h = 0;
		for (int i = 0; i < variables.length; i++) {
			h += (variables[i].hashCode() * (values[i] ? 1 : 2));
		}
		return h;
	}
}
