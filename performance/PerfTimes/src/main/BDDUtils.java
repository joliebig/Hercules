package main;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;

import net.sf.javabdd.BDD;

public class BDDUtils<T> {

	public static BDD someOf(BDD[] bdds) {
		BDD ret = BddManager.falseFormula;
		for (BDD x : bdds) {
			ret = ret.or(x);
		}
		return ret;
	}

	public static BDD noneOf(BDD[] bdds) {
		BDD ret = BddManager.trueFormula;
		for (BDD x : bdds) {
			ret = ret.and(x.not());
		}
		return ret;
	}

	public static BDD oneOfOrNone(BDD[] bdds) {
		return noneOf(bdds).or(BDDUtils.oneOf(bdds));
	}

	public static BDD[] filterFromArray(BDD bdd, BDD[] bdds) {
		BDD[] ret = new BDD[bdds.length-1];
		int retIndex = 0;
		for (int i = 0; i < bdds.length; i++) {
			if (bdds[i] != bdd) {
				assert (retIndex < ret.length) : "Element not found in array";
				ret[retIndex++] = bdds[i];
			}
		}
		return ret;
	}

	public static BDD oneOf(BDD[] bdds) {
		BDD ret = BddManager.falseFormula;
		for (BDD elem : bdds) {
			ret = ret.or(elem.and(noneOf(filterFromArray(elem, bdds))));
		}
		return ret;
	}
	public static BDD efficientOrWithAll(List<BDD> featureModel) {
		return efficientOrAll_rec(featureModel, 0, featureModel.size()-1);
	}
	private static BDD efficientOrAll_rec(List<BDD> featureModel,int start, int end) {
		if (start==end) return featureModel.get(start).id();
		else if (start > end) return BddManager.falseFormula.id();
		else {
			int mid = (start + end) / 2;
			BDD ret = efficientOrAll_rec(featureModel, start, mid);
			ret.orWith(efficientOrAll_rec(featureModel, mid+1, end));
			return ret;
		}
	}

	public static BDD existUncommonVars(BDD x, BDD[] bdds) {
		BDD ret = x;
		int[][] varProfiles = new int[bdds.length][];
		for (int i = 0; i < varProfiles.length; i++)
			varProfiles[i] = bdds[i].varProfile();
		int[] xProfile = x.varProfile();
		for (int i = 0; i < xProfile.length; i++) {
			BDD var = BddManager.factory.ithVar(i);
			if (BddManager.BDDToVar.containsKey(var)) {
				//System.out.println("Name: " + BddManager.BDDToString(var, false, false) + " x:" + xProfile[i] + " other[0]:"+varProfiles[0][i]);
				if (xProfile[i] > 0) {
					boolean missingInOne = false;
					for (int j = 0; j < varProfiles.length && missingInOne==false; j++)
						if (varProfiles[j][i] == 0)
							missingInOne = true;
					if (missingInOne)
						ret = ret.exist(BddManager.factory.ithVar(i));
				}
			}
		}
		return ret;
	}

	public static BDD andAll(Set<BDD> config) {
		BDD ret = BddManager.trueFormula;
		//int id = 0;
		for (BDD x : config) {
			//System.out.println("and " + (id++) + "/"+config.size());
			ret = ret.and(x);
		}
		return ret;
	}

	public static BDD conjunctVarsInConfigAndNegateOthers(Set<BDD> config, Set<BDD> allVars) {
		BDD ret = BddManager.trueFormula;
		for (BDD x : allVars)
			if (config.contains(x))
				ret = ret.and(x);
			else
				ret = ret.and(x.not());
		return ret;
	}

	public static Set<String> getUsedVarNames(BDD bdd) {
		Set<String> ret = new HashSet<>();
		int[] varProfile = bdd.varProfile();
		for (int i =0; i < varProfile.length; i++) {
			if(varProfile[i]>0)
				ret.add(BddManager.BDDToVar.get(BddManager.factory.ithVar(i)));
		}
		return ret;
	}

	public static <T> String setToString(Set<T> set) {
		if (set.size()==0) return "()";
		StringBuilder sb = new StringBuilder();
		for (T elem : set) {
			sb.append(elem.toString());
			sb.append(", ");
		}
		// remove last ", "
		sb.replace(sb.length()-2, sb.length(), "");
		sb.append(")");
		sb.insert(0, "(");
		return sb.toString();
	}

	public static <T> String arrayToString(T[] arr) {
		if (arr.length==0) return "()";
		StringBuilder sb = new StringBuilder();
		for (T elem : arr) {
			sb.append(elem.toString());
			sb.append(", ");
		}
		// remove last ", "
		sb.replace(sb.length()-2, sb.length(), "");
		sb.append(")");
		sb.insert(0, "(");
		return sb.toString();
	}
	public static String arrayToString(byte[] arr) {
		if (arr.length==0) return "()";
		StringBuilder sb = new StringBuilder();
		for (byte elem : arr) {
			sb.append(elem);
			sb.append(", ");
		}
		// remove last ", "
		sb.replace(sb.length()-2, sb.length(), "");
		sb.append(")");
		sb.insert(0, "(");
		return sb.toString();
	}
	/**
	 * Iterates through solutions for the BDD.
	 * next() returns a byte array which contains 1,0, or -1 for all bdd variables.
	 * 1 means the var is set to true in the bdd, 0 means false, -1 means don't care.
	 * If a dead end is hit in the last iteration, next might return null! take care!
	 * There is only one array instance which is reused in each next() call.
	 * @author rhein
	 */
	public  static class BDDAllsatIterator implements Iterable<byte[]> ,Iterator<byte[]> {
		BDD subject;
		Stack<Pair<BDD, Boolean>> todo = new Stack<>();
		// boolean means "high path done"
		byte[] currentSol = null;
		public BDDAllsatIterator (BDD bdd) {
			this.subject=bdd;
			currentSol = new byte[BddManager.factory.varNum()];
			todo.push(new Pair<BDD, Boolean>(subject, false));
		}
		@Override
		public boolean hasNext() {
			return !todo.isEmpty();
		}
		
		@Override
		public byte[] next() {
			boolean cont = true;
			Pair<BDD, Boolean> x = todo.pop();
			BDD current = x._1;
			boolean highpathDone = x._2;
			while (cont) {
				/*
				if (current.isOne())
					System.out.println("staring loop with TRUE");
				else if  (current.isZero())
					System.out.println("staring loop with FALSE");
				else
					System.out.println("staring loop with " + BddManager.BDDToString(BddManager.factory.ithVar(current.var()),false,false) +  " " + (highpathDone?"lowpath":"highpath"));
				*/
				if (current.isOne()) {
					cont = false;
				} else if (current.isZero()) {
					// this is a dead end. Backtrack
					//System.out.println("dead end");
					if (todo.isEmpty()) {
						//System.out.println("done");
						return null;
					} else {
						
						x = todo.pop();
						current = x._1;
						highpathDone = x._2;
						//System.out.println("loading " + BddManager.BDDToString(BddManager.factory.ithVar(current.var()),false,false));
					}
				} else {
					if (highpathDone) {
						//System.out.println("lowpath");
						currentSol[current.var()] = 0;
						if (current.low().isOne()) { // fill with don't cares to the end
							//System.out.println("next is one");
							Arrays.fill(currentSol, current.var()+1, currentSol.length, (byte)-1);
						} else if (!current.low().isZero()) // fill with don't cares until next used var (if the next is zero, it does not matter)
							for (int i = current.var()+1; i < current.low().var(); i++)
								currentSol[i] = -1;
						current = current.low();
						highpathDone=false;
					} else {
						//System.out.println("highpath");
						todo.push(new Pair<BDD, Boolean>(current, true));
						currentSol[current.var()] = 1;
						if (current.high().isOne()) { // fill with don't cares to the end
							//System.out.println("next is one");
							Arrays.fill(currentSol, current.var()+1, currentSol.length, (byte)-1);
						} else if (!current.high().isZero()) // fill with don't cares until next used var (if the next is zero, it does not matter)
							for (int i = current.var()+1; i < current.high().var(); i++)
								currentSol[i] = -1;
						current = current.high();
						highpathDone=false;
					}
				}
				//System.out.println("sol after loop: " + BDDUtils.arrayToString(currentSol));
			}
			return currentSol;
		}
		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}
		@Override
		public Iterator<byte[]> iterator() {
			return new BDDAllsatIterator(subject);
		}
	}
}
