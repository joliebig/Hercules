package nf;

public class NF_literal {
	private NF_variable var;
	private boolean positive;
	NF_literal(NF_variable var, boolean positive) {
		this.var = var;
		this.positive = positive;
	}
	public NF_variable getVariable() {
		return var;
	}
	public boolean isPositive() {
		return positive;
	}
	@Override
	public int hashCode() {
		return var.hashCode() * (positive?1:2);
	}
	@Override
	public boolean equals(Object obj) {
		if (! (obj instanceof NF_literal))
			return false;
		else {
			NF_literal rhs = (NF_literal) obj;
			if (positive != rhs.positive)
				return false;
			else {
				return var.equals(rhs.var);
			}
		}
	}

}
