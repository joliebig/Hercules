package expressionParser;

import java.io.InputStream;
import java.io.Reader;

public class ScannerCreator {
	public static FeatureModelScanner createScanner (Reader in) {
	    return new FeatureModelScanner(in);
	  }
	public static FeatureModelScanner createScanner (InputStream in) {
	    return new FeatureModelScanner(in);
	  }
}
