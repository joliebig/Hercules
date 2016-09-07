package main;

import java.io.FileReader;
import java.io.File;

import net.sf.javabdd.BDD;
import expressionParser.FeatureModelParser;
import expressionParser.ScannerCreator;
import DimacsParser;

public class Main {

	public static void main(String[] args) throws Exception {
		if (args.length == 1) {
			File file = new File(args[0]);
			if (file.exists() && !file.isDirectory()) {
				FeatureModelParser parser = new FeatureModelParser(ScannerCreator.createScanner(new FileReader(args[0])));
				System.out.println("Loading model");
				BDD model = (BDD)parser.parse().value;
				
				
				
				System.out.println("Printing models");
				System.out.println(BddManager.BDDToString(model, false, false));
				
				System.out.println("Printing satOne");
				System.out.println(BddManager.BDDToString(model.satOne(), false, false));
			}
		}		
	}
}
