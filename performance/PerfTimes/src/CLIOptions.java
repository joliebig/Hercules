/**
 * Command line options.
 */
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.List;
import java.util.logging.*;

import org.apache.commons.cli.*;
public class CLIOptions {
    private static final Logger log = Logger.getLogger(CLIOptions.class.getName());
    private String[] args = null;
    private Options options = new Options();
    private int SQLITE_PREDICTION_SCENARIO;
    private final List<String> VALID_MODES = Arrays.asList("featurewise", "pairwise", "codecoverage", "randomsampling", "allyes");
    private String INPUT_MODE;
    private String PREDICT_MODE;
    //private final String TYPECHEF_SQLITE_IFDEFTOIF_DIR = new File(System.getProperty("user.dir")).getParentFile().getParent()
            //+ File.separator + "TypeChef-SQLiteIfdeftoif" + File.separator + "optionstructs_ifdeftof";
    //private final String TYPECHEF_SQLITE_IFDEFTOIF_DIR = new File(System.getProperty("user.dir")).getParentFile().getParent()
            //+ File.separator + "TypeChef" + File.separator + "TypeChef-SQLiteIfdeftoif" + File.separator + "optionstructs_ifdeftoif";

    private String getSQLiteDir() {
        File current;
        current = new File(System.getProperty("user.dir"));
        while (!current.getName().equals("Hercules")) {
            current = current.getParentFile();
        }
        return current.getParent() + File.separator + "TypeChef-SQLiteIfdeftoif" + File.separator + "optionstructs_ifdeftoif";
    }

    private String validModesToString() {
        StringBuilder sb = new StringBuilder();
        String seperator = "";
        sb.append("[");
        for (String current : VALID_MODES) {
            sb.append(seperator);
            seperator = "/";
            sb.append(current);
        }
        sb.append("]");
        return sb.toString();
    }

    public CLIOptions(String[] args) {
        this.args = args;
        options.addOption("h", "help", false, "display help");
        options.addOption("cs", "case study", true, "Set case study: [sqlite]");
        options.addOption("f", "folder", true, "Folder location of result files");
        options.addOption("im", "inputmode", true, "Set input mode: [featurewise/pairwise/codecoverage/randomsampling]");
        options.addOption("pm", "predictmode", true, "Set predict mode: " + validModesToString());
        options.addOption("rc", "randomconfig", true, "Generate random config from random.csv");
    }

    public void parse() {
        PerfPrediction predict = new PerfPrediction();
        CommandLineParser parser = new DefaultParser();
        CommandLine cmd = null;
        try {
            cmd = parser.parse(options, args);

            if (cmd.hasOption("h")) {
                help();
            }

            if (cmd.getOptions().length == 0) {
                try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
                    predict.add(reader, "");
                    predict.printFixedValues();
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return;
            }

            if (cmd.hasOption("cs") && cmd.getOptionValue("cs").equals("sqlite")) {
                if (cmd.hasOption("f")) {
                    File location = new File(cmd.getOptionValue("f"));
                    if (location.isDirectory()) {
                        setSQLitePredictionScenarioNo(location);
                        if (cmd.hasOption("im")) {
                            if (isValidMode(cmd.getOptionValue("im"))) {
                                if (cmd.hasOption("pm")) {
                                    if (isValidMode(cmd.getOptionValue("pm"))) {
                                        this.INPUT_MODE = cmd.getOptionValue("im");
                                        this.PREDICT_MODE = cmd.getOptionValue("pm");
                                        switch (this.INPUT_MODE) {
                                            case "featurewise":
                                                try {
                                                    predict.addFeatureWise(location);
                                                } catch (Exception e) {
                                                    e.printStackTrace();
                                                }
                                        }
                                        predict.predict(this.PREDICT_MODE, getSQLiteDir(), location);

                                    } else {
                                        log.log(Level.SEVERE, "invalid mode for option '-pm [featurewise/pairwise/codecoverage/randomsampling]'");
                                    }
                                } else {
                                    log.log(Level.SEVERE, "missing prediction mode '-pm [featurewise/pairwise/codecoverage/randomsampling]'");
                                }
                            } else {
                                log.log(Level.SEVERE, "invalid mode for option '-im [featurewise/pairwise/codecoverage/randomsampling]'");
                            }
                        } else {
                            log.log(Level.SEVERE, "missing input mode '-im [featurewise/pairwise/codecoverage/randomsampling]'");
                        }

                    } else {
                        log.log(Level.SEVERE, "'-f <location>' does not point to a folder", cmd.getOptionValue("f"));
                    }
                } else {
                    log.log(Level.SEVERE, "Use option '-f <location>'", cmd.getOptionValue("f"));
                }
            }
            if (cmd.hasOption("rc")) {
                GenerateRandomConfig.generateConfigs(cmd.getOptionValue("rc"));
            }

        } catch (ParseException e) {
            log.log(Level.SEVERE, "Failed to parse comand line properties", e);
            help();
        }
    }

    private void setSQLitePredictionScenarioNo(File folder) {
        this.SQLITE_PREDICTION_SCENARIO = Integer.parseInt(folder.getName());
    }

    private Boolean isValidMode(String mode) {
        return VALID_MODES.contains(mode);
    }

    private void help() {
        HelpFormatter formatter = new HelpFormatter();
        formatter.setWidth(90);
        formatter.printHelp("PerfTimes.jar", options);
        System.exit(0);
    }
}
