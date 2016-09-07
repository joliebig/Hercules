package nf;

import java.util.Collection;

public interface NF_clause {
	public Collection<NF_literal> getLiterals();
	public int getSize();
	public Collection<NF_variable> getVariables();
}

