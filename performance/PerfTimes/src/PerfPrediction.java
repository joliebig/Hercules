import expressionParser.FeatureModelParser;
import expressionParser.ScannerCreator;
import net.sf.javabdd.BDD;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

/**
 * Created by flo on 16/08/16.
 */
public class PerfPrediction {
    public PerfPrediction() {
        
    }

    private final static String BASE_NAME = "BASE";
    private final static String HERCULES_START_MESSAGE = "-- Hercules Performance --";
    private final static String HERCULES_END_MESSAGE = "-- Hercules Performance End --";
    private final static int EPSILON = 10;
    private final static Boolean ASCENDING = true;
    private final static Boolean DESCENDING = false;
    private final static Boolean FILTER = true;
    private final static Double FILTER_THRESHOLD = 0.0005;
    private final static String NO_LOCATION = "";
    private final static String CSV_HEADER = "feature,id,time\n";
    private static Boolean PRINT_HASHMAP = false;
    private final static String PERF_FILE_PREFIX_FEATUREWISE = "perf_ft_";
    private final static String PERF_FILE_PREFIX_PAIRWISE = "perf_pr_";
    private final static String PERF_FILE_PREFIX_CODECOVERAGE = "perf_cc_";
    private final static String PERF_FILE_PREFIX_RANDOM = "perf_rnd_";
    private final static String PERF_FILE_NAME_ALLYES = "perf_ay.txt";
    private final static String CONFIG_PREFIX_PAIRWISE = "Prod";
    private final static String CONFIG_PREFIX_FEATUREWISE = "id2i_include_";
    private final static String TMP_FILE = File.separator + "perf_content.tmp";
    private final static String END = "End ";
    private final static String MEASUREMENT_COUNTER = "Measurement counter: ";
    private final static String SQLITE_START = "With SQLite";
    private final static String TOTAL_TIME = "Total time: ";
    private static PerformanceModel SOURCE_MODEL = new PerformanceModel();

    public static class TimeContent implements Comparable<TimeContent> {
        private String additionalContent;
        private Double originalTime;
        private Double finalTime;
        private final Double outerTime;
        private List<Double> allTimes = new ArrayList<Double>();

        /**
         * TimeContent stores time information for a given feature. During the first run we have to post process the
         * given time information to account for nested time measurements. Time measured for feature 'A' includes the
         * nested time measured for feature 'A#B'.
         *
         * @param feature
         * @param original
         * @param finalT
         * @param outerT
         */
        public TimeContent(String feature, Double original, Double finalT, double outerT) {
            this.additionalContent = feature;
            this.originalTime = original;
            this.finalTime = finalT;
            this.outerTime = outerT;
            this.allTimes.add(original);
        }

        /*        public TimeContent(int measurements, double timeSum) {
                    this.additionalContent = "ms";
                    this.measurementCounter = measurements;
                    this.totalTimeSum = timeSum;
                    this.finalTime = this.totalTimeSum / (double) this.measurementCounter;
                }
        
                public int getMeasurementCounter() {
                    return this.measurementCounter;
                }
        
                public double getTotalTimeSum() {
                    return this.totalTimeSum;
                }
        */
        public Double getOriginalTime() {
            return this.originalTime;
        }

        public Double getOuterTime() {
            return this.outerTime;
        }

        public List<Double> getAllTimes() {
            return this.allTimes;
        }

        public double getFinalTime() {
            if (this.allTimes.size() > 1) {
                return average(this.allTimes);
            } else {
                return this.finalTime;
            }
        }

        public double getVariance() {
            if (this.allTimes.size() > 1) {
                return variance(this.allTimes);
            } else {
                return 0.0;
            }
        }

        public void updateFinalTime(Double newTime) {
            if (this.allTimes.size() > 1) {
                System.out.println("ERROR: updating final time on a set with multiple times!");
            }
            this.finalTime = newTime;
            this.allTimes.clear();
            this.allTimes.add(newTime);
        }

        public TimeContent update(TimeContent otherContent) {
            this.additionalContent = "ms";
            this.allTimes.addAll(otherContent.allTimes);
            return this;
        }

        public String toString() {
            if (this.allTimes.size() > 1) {
                return String.format("%.6f %s, deviation: %.2f%% %s", this.getFinalTime(), this.additionalContent, deviation(this.allTimes), listToString(this.allTimes));
                //return String.format("%.6f %s, deviation: %.2f%%", this.getFinalTime(), this.additionalContent, deviation(this.allTimes));
            } else {
                return String.format("%.6f %s", this.finalTime, this.additionalContent);
            }
        }

        private Double average(List<Double> list) {
            Double sum = 0.0;
            for (Double current: list) {
                sum += current;
            }
            return sum / list.size();
        }

        /**
         * Calculates the ± variance from the average value in our List this.allTimes.
         *
         * @return
         */
        private Double variance(List<Double> list) {
            Double average = average(list);
            Double sum = 0.0;
            Double count = 0.0;
            for (Double current: list) {
                sum += (current - average)*(current - average);
                count++;
            }
            return (Math.sqrt((sum / count)));
        }

        /**
         * Calculates deviation between the lowest and highest value in given list.
         *
         * @param list
         * @return
         */
        private Double deviation(List<Double> list) {
            Double min = Collections.min(list);
            Double max = Collections.max(list);
            return Math.abs((max - min) * 100 / ((max + min) / 2));
        }

        private String listToString(List<Double> list) {
            StringBuilder str = new StringBuilder();
            str.append("[");
            String prefix = "";
            for (Double current: list) {
                str.append(prefix);
                prefix = ", ";
                str.append(String.format("%.6f", current));
            }
            str.append("]");
            return str.toString();
        }

        @Override
        public int compareTo(TimeContent other) {
            return Double.compare(this.getFinalTime(), other.getFinalTime());
        }
    }

    public static class PerformanceRun {
        private HashMap<String, TimeContent> hashmap = new HashMap<>();
        private int numberOfMeasurements = 0;
        private int numberOfModels = 1;
        private ArrayList<String> tests = new ArrayList<>();
        private double totalTime = 0.0;

        public HashMap<String, TimeContent> getHashmap() {
            return hashmap;
        }

        public int getNumberOfMeasurements() {
            return this.numberOfMeasurements;
        }

        public void setNumberOfMeasurements(int numberOfMeasurements) {
            this.numberOfMeasurements = numberOfMeasurements;
        }

        public int getNumberOfModels() {
            return this.numberOfModels;
        }

        public ArrayList<String> getTests() {
            return this.tests;
        }

        public void setTests(ArrayList<String> tests) {
            this.tests = tests;
        }

        public void setTotalTime(Double totalTime) {
            this.totalTime = totalTime;
        }

        public Double getTotalTime() {
            return this.totalTime;
        }

        public Boolean equalTests(PerformanceRun other) {
            if (this.tests.size() == other.getTests().size()) {
                for (String currentTest : this.tests) {
                    if (!other.getTests().contains(currentTest)) {
                        return false;
                    }
                }
                return true;
            }
            return false;
        }
    }

    public static class PerformanceModel {
        private ArrayList<PerformanceRun> performanceRuns = new ArrayList<>();
        private PerformanceRun targetRun = null;
        private HashMap<String, TimeContent> hashmap = new HashMap<>();
        private BDD configuration;
        private Double predictedTime = Double.MIN_VALUE;
        private Double predictedVariance = Double.MIN_VALUE;

        private void resetValues() {
            this.predictedTime = Double.MIN_VALUE;
            this.predictedVariance = Double.MIN_VALUE;
        }

        public int getNumberOfRuns() {
            return this.performanceRuns.size();
        }

        public Double getPredictedTime() {
            if (this.predictedTime == Double.MIN_VALUE) {
                try {
                    this.computePredictions();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            return this.predictedTime;
        }

        public Double getPredictedVariance() {
            if (this.predictedVariance == Double.MIN_VALUE) {
                try {
                    this.computePredictions();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
            return this.predictedVariance;
        }

        public void setTargetRun(PerformanceRun targetRun) {
            this.targetRun = targetRun;
        }

        public PerformanceRun getPerfRun(int index) {
            if (index < this.performanceRuns.size()) {
                return this.performanceRuns.get(index);
            }
            return null;
        }

        private void computePredictions() throws Exception {
            this.predictedTime = 0.0;
            this.predictedVariance = 0.0;

            for (String current: this.getHashmap().keySet()) {
                String currentModified = current.replaceAll("#", " and ").replaceAll("&&", " and ").replaceAll("\\|\\|", "or");
                FeatureModelParser parser = new FeatureModelParser(ScannerCreator.createScanner(new StringReader(currentModified)));
                BDD currentBDD = (BDD) parser.parse().value;
                if (!currentBDD.and(configuration).isZero()) {

                    this.predictedTime += hashmap.get(current).getFinalTime();
                    this.predictedVariance += hashmap.get(current).getVariance();
                }
            }
        }

        public void setBDD(BDD config) {
            resetValues();
            this.configuration = config;
        }

        public void addPerformanceRun(PerformanceRun newRun) {
            this.performanceRuns.add(newRun);
        }

        public HashMap<String, TimeContent> getHashmap() {
            if (this.hashmap.isEmpty()) {
                compute();
            }
            return this.hashmap;
        }

        private void compute() {
            if (performanceRuns.size() == 1) {
                this.hashmap = performanceRuns.get(0).getHashmap();
            } else {
                for (PerformanceRun currentRun : this.performanceRuns) {
                    // if (targetRun.equalTests(currentRun))
                    HashMap<String, TimeContent> currentHashmap = currentRun.getHashmap();
                    for (String current : currentHashmap.keySet()) {
                        if (!hashmap.containsKey(current)) {
                            hashmap.put(current, currentHashmap.get(current));
                        } else {
                            hashmap.put(current, hashmap.get(current).update(currentHashmap.get(current)));
                            //myCompleteMap.put(current, update(myCompleteMap.get(current), currentMap.get(current)));
                        }
                    }
                }
            }
            this.hashmap = sortByValues(this.hashmap, DESCENDING);

        }
    }

    static private BDD getBDDfromConfigFile(String location) {
        try {
            byte[] encoded = Files.readAllBytes(Paths.get(location));
            String content = new String(encoded).replaceAll("\n#undef ([A-Za-z0-9_])", " and !$1").replaceAll("\n#define ([A-Za-z0-9_])", " and $1").replaceAll("#undef ([A-Za-z0-9_])", "!$1").replaceAll("#define ([A-Za-z0-9_])", "$1");
            FeatureModelParser parser = new FeatureModelParser(ScannerCreator.createScanner(new StringReader(content)));
            return (BDD)parser.parse().value;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    static private String getFileNameWithoutExtension(File file) {
        String fileName = file.getAbsolutePath();
        int pos = fileName.lastIndexOf(".");
        if (pos > 0) {
            fileName = fileName.substring(0, pos);
        }
        return fileName;
    }

    static private String getFileNameWithCSVExtension(File file) {
        return getFileNameWithoutExtension(file) + ".csv";
    }

    static private String getFileNameWithSecondCSVExtension(File file) {
        if (file.isDirectory()) {
            return "featureAggregations.csv";
        } else {
            return getFileNameWithoutExtension(file) + "_featureAggregations.csv";
        }
    }

    static private Double predict(PerformanceModel perfModel, BDD configuration) {
        perfModel.setBDD(configuration);
        Double currentPredictedTime = perfModel.getPredictedTime();
        Double currentVariance = perfModel.getPredictedVariance();
        if (currentVariance != 0.0) {
            System.out.println(String.format("Predicted: %.6f ms ± %.6f ms", currentPredictedTime, currentVariance));
        } else {
            System.out.println(String.format("Predicted: %.6f ms", currentPredictedTime));
        }
        return currentPredictedTime;
    }

    static public void predict(String predictMode, String structLocation, File location) {
        switch (predictMode) {
            case "allyes":
                if (new File(structLocation).isDirectory()) {
                    BDD configuration = getBDDfromConfigFile(structLocation + "/allyes/allyes_include.h");
                    predict(SOURCE_MODEL, configuration);
                    File resultFile = new File(getResultFile(location, new File(NO_LOCATION), predictMode));
                    if (resultFile.exists()) {
                        System.out.println(getResultTime(resultFile) + "\n");
                    }
                } else {
                    // TODO
                }
                break;
            case "codecoverage":
            case "random":
            case "featurewise":
            case "pairwise":
                if (new File(structLocation).isDirectory()) {
                    for (File child : getConfigurationFiles(structLocation, predictMode)) {
                        if (isConfigFile(child, predictMode)) {
                            BDD configuration = getBDDfromConfigFile(child.toString());
                            predict(SOURCE_MODEL, configuration);
                            File resultFile = new File(getResultFile(location, child, predictMode));
                            if (resultFile.exists()) {
                                System.out.println(getResultTime(resultFile) + "\n");
                            }
                        }
                    }
                }
                break;
            default:
                break;
        }
    }

    public static String getResultTime(File file) {
        return tail2(file, 2).split(System.lineSeparator(), 2)[0];
    }

    public static String tail2( File file, int lines) {
        java.io.RandomAccessFile fileHandler = null;
        try {
            fileHandler =
                    new java.io.RandomAccessFile( file, "r" );
            long fileLength = fileHandler.length() - 1;
            StringBuilder sb = new StringBuilder();
            int line = 0;

            for(long filePointer = fileLength; filePointer != -1; filePointer--){
                fileHandler.seek( filePointer );
                int readByte = fileHandler.readByte();

                if( readByte == 0xA ) {
                    if (filePointer < fileLength) {
                        line = line + 1;
                    }
                } else if( readByte == 0xD ) {
                    if (filePointer < fileLength-1) {
                        line = line + 1;
                    }
                }
                if (line >= lines) {
                    break;
                }
                sb.append( ( char ) readByte );
            }

            String lastLine = sb.reverse().toString();
            return lastLine;
        } catch( java.io.FileNotFoundException e ) {
            e.printStackTrace();
            return null;
        } catch( java.io.IOException e ) {
            e.printStackTrace();
            return null;
        }
        finally {
            if (fileHandler != null )
                try {
                    fileHandler.close();
                } catch (IOException e) {
                }
        }
    }

    static public PerformanceModel execute(BufferedReader bufReader, String location) throws IOException {
        HashMap<String, TimeContent> myCompleteMap = new HashMap<String, TimeContent>();

        String line;
        String splitter = " -> ";
        Boolean started = false;
        int runCounter = 0;
        PerformanceModel perfModel = new PerformanceModel();

        PerformanceRun currentRun = null;
        ArrayList<String> currentTests = null;
        HashMap<String, TimeContent> currentMap = null;
        while ((line = bufReader.readLine()) != null) {
            if (started) {
                if (line.startsWith(MEASUREMENT_COUNTER)) {
                    HashMap<String, TimeContent> featureMap = new HashMap<String, TimeContent>();
                    currentMap = featureMap;
                    PerformanceRun model = new PerformanceRun();
                    currentRun = model;
                    perfModel.addPerformanceRun(currentRun);
                    currentRun.setNumberOfMeasurements(Integer.parseInt(line.split(MEASUREMENT_COUNTER)[1]));
                    if (currentTests != null) {
                        currentRun.setTests(currentTests);
                    }
                } else if (line.contains(splitter)) {
                    String[] featureAndTime = line.split(splitter);
                    String featureName = featureAndTime[0];
                    featureAndTime = featureAndTime[1].split(" ", 2);
                    Double time = Double.parseDouble(featureAndTime[0]);
                    featureAndTime = featureAndTime[1].split("ms, ", 2)[1].split(" ", 2);
                    Double outerTime = Double.parseDouble(featureAndTime[0]);
                    TimeContent timeContent = new TimeContent(featureAndTime[1], time, time, outerTime);
                    currentRun.getHashmap().put(featureName, timeContent);
                } else if (line.equals(HERCULES_END_MESSAGE)) {
                    started = false;
                    String[] keyArray = currentRun.getHashmap().keySet().toArray(new String[currentRun.getHashmap().size()]);
                    for (int i = 0; i < currentRun.getHashmap().size(); i++) {
                        double currentTime = currentRun.getHashmap().get(keyArray[i]).getOriginalTime();
                        for (int j = 0; j < currentRun.getHashmap().size(); j++) {
                            if (i != j && isSuccessor(keyArray[i], keyArray[j])) {
                                /*if (keyArray[i].equals(BASE_NAME)) {
                                    System.out.println("ERROR: " + currentTime + " <-> " + keyArray[j]);
                                }*/
                                currentTime -= currentRun.getHashmap().get(keyArray[j]).getOuterTime();
                                currentTime -= currentRun.getHashmap().get(keyArray[j]).getOriginalTime();
                            }
                        }
                        currentRun.getHashmap().get(keyArray[i]).updateFinalTime(currentTime);
                    }

                    /*Double total_time = 0.0;
                    for (String current : currentMap.keySet()) {
                        total_time += currentMap.get(current).getFinalTime();
                        if (!myCompleteMap.containsKey(current)) {
                            myCompleteMap.put(current, currentMap.get(current));
                        } else {
                            myCompleteMap.put(current, myCompleteMap.get(current).update(currentMap.get(current)));
                            //myCompleteMap.put(current, update(myCompleteMap.get(current), currentMap.get(current)));
                        }
                        if (location == NO_LOCATION) {
                            System.out.println(current + " -> " + currentMap.get(current));
                        }
                    }
                    if (PRINT_HASHMAP) {
                        System.out.println(String.format("Total time: %.6f", total_time));
                    }*/
                    currentRun = null;
                    currentTests = null;
                    currentMap = null;
                } else if (line.startsWith(TOTAL_TIME)) {
                    Scanner st = new Scanner(line);
                    while (!st.hasNextDouble()) {
                        st.next();
                    }
                    currentRun.setTotalTime(st.nextDouble());
                } else {
                    //System.out.println(line);
                }
            } else if (line.equals(HERCULES_START_MESSAGE)) {
                runCounter++;
                started = true;
            } else if (line.startsWith(MEASUREMENT_COUNTER)) {
                System.out.print(line.split("Performance counter: ")[1] + " ");
            } else if (line.startsWith(END)) {
                currentTests.add(line.split(END)[1]);
            } else if (line.startsWith(SQLITE_START)) {
                ArrayList<String> tests = new ArrayList<>();
                currentTests = tests;
            }
        }
        myCompleteMap = perfModel.getHashmap();
        myCompleteMap = sortByValues(myCompleteMap, DESCENDING);

        StringBuilder str = new StringBuilder();
        StringBuilder featureAggregations = new StringBuilder();
        String prefix = "";
        for (String current: myCompleteMap.keySet()) {
            if (FILTER && myCompleteMap.get(current).getFinalTime() > FILTER_THRESHOLD) {
                int id = 0;
                for (Double currentTime: myCompleteMap.get(current).getAllTimes()) {
                    str.append(prefix);
                    featureAggregations.append(prefix);

                    prefix = "\n";

                    featureAggregations.append(String.format("%d,%d,%.6f", getNumberOfDividers(current), id, currentTime));
                    str.append(String.format("%s,%d,%.6f", current, id++, currentTime));
                }
                if (PRINT_HASHMAP) {
                    System.out.println(current + " -> " + myCompleteMap.get(current));
                }
            } else if (!FILTER) {
                int id = 0;
                for (Double currentTime: myCompleteMap.get(current).getAllTimes()) {
                    str.append(prefix);
                    featureAggregations.append(prefix);

                    prefix = "\n";

                    featureAggregations.append(String.format("%d,%d,%.6f", getNumberOfDividers(current), id, currentTime));
                    str.append(String.format("%s,%d,%.6f", current, id++, currentTime));
                }
                if (PRINT_HASHMAP) {
                    System.out.println(current + " -> " + myCompleteMap.get(current));
                }
            }
        }

        if (runCounter > 1 && !location.equals(NO_LOCATION)) {
            PrintWriter writer = new PrintWriter(location, "UTF-8");
            writer.print(CSV_HEADER);
            writer.print(str.toString());
            writer.close();
            writer = new PrintWriter(getFileNameWithSecondCSVExtension(new File(location)), "UTF-8");
            writer.print(CSV_HEADER);
            writer.print(featureAggregations.toString());
            writer.close();
        }
        return perfModel;
    }

    static public void printFixedValues() {
        StringBuilder sb = new StringBuilder();
        String sep = "";
        for (String current : SOURCE_MODEL.getHashmap().keySet()) {
            sb.append(sep);
            sep = "\n";
            sb.append(current + " -> " + SOURCE_MODEL.getHashmap().get(current));
        }
        System.out.println(sb.toString());
    }

    static public Boolean isCorrectFile(File file, String predictionMode) {
        Boolean result;
        switch (predictionMode) {
            case "random":
                result =  file.getName().startsWith(PERF_FILE_PREFIX_RANDOM);
                break;
            case "featurewise":
                result =  file.getName().startsWith(PERF_FILE_PREFIX_FEATUREWISE);
                break;
            case "pairwise":
                result =  file.getName().startsWith(PERF_FILE_PREFIX_PAIRWISE);
                break;
            case "allyes":
                result =  file.getName().startsWith(PERF_FILE_NAME_ALLYES);
                break;
            case "codecoverage":
                result =  file.getName().startsWith(PERF_FILE_PREFIX_CODECOVERAGE);
                break;
            default:
                result =  false;
        }
        return result;
    }

    static public File[] getConfigurationFiles(String structLocation, String predictMode) {
        File[] result;
        switch (predictMode) {
            case "pairwise":
                result = new File(structLocation + "/pairwise/generated").listFiles();
                break;
            case "featurewise":
                result = new File(structLocation + "/featurewise/generated").listFiles();
                break;
            case "random":
                result = new File(structLocation + "/random/generated").listFiles();
                break;
            case "codecoverage":
                result = new File(structLocation + "/codecoverage/generated").listFiles();
                break;
            case "allyes":
                result = new File[]{new File(structLocation + "/allyes/allyes_include.h")};
                break;
            default:
                result = new File[0];
                break;
        }
        return result;
    }

    static public String getResultFile(File location, File configFile, String predictMode) {
        String id;
        String resultFile;
        switch (predictMode) {
            case "featurewise":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_FEATUREWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_FEATUREWISE + id + ".txt";
            case "pairwise":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_PAIRWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_PAIRWISE + id + ".txt";
            case "random":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_FEATUREWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_RANDOM + id + ".txt";
            case "allyes":
                return location.getAbsolutePath() + File.separator + PERF_FILE_NAME_ALLYES;
            case "codecoverage":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_FEATUREWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_CODECOVERAGE + id + ".txt";
            default:
                resultFile = "";
                break;
        }
        return resultFile;
    }

    static public Boolean isConfigFile(File file, String predictMode) {
        Boolean result;
        switch (predictMode) {
            case "codecoverage":
            case "random":
            case "featurewise":
                result = file.getName().startsWith(CONFIG_PREFIX_FEATUREWISE);
                break;
            case "pairwise":
                result = file.getName().startsWith(CONFIG_PREFIX_PAIRWISE);
                break;
            default:
                result = false;
                break;
        }
        return result;
    }

    static public void addPrediction(File location, String predictionMode) throws Exception {
        for (File child : location.listFiles()) {
            if (isCorrectFile(child, predictionMode)) {
                try (BufferedReader reader = new BufferedReader(new FileReader(child))) {
                    add(reader, location.toString());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        SOURCE_MODEL.compute();
    }

    static public void exportCSV(String location) {

        HashMap<String, TimeContent> myCompleteMap = SOURCE_MODEL.getHashmap();
        myCompleteMap = sortByValues(myCompleteMap, DESCENDING);

        StringBuilder str = new StringBuilder();
        StringBuilder featureAggregations = new StringBuilder();
        String prefix = "";
        for (String current: myCompleteMap.keySet()) {
            if (FILTER && myCompleteMap.get(current).getFinalTime() > FILTER_THRESHOLD) {
                int id = 0;
                for (Double currentTime: myCompleteMap.get(current).getAllTimes()) {
                    str.append(prefix);
                    featureAggregations.append(prefix);

                    prefix = "\n";

                    featureAggregations.append(String.format("%d,%d,%.6f", getNumberOfDividers(current), id, currentTime));
                    str.append(String.format("%s,%d,%.6f", current, id++, currentTime));
                }
                if (PRINT_HASHMAP) {
                    System.out.println(current + " -> " + myCompleteMap.get(current));
                }
            } else if (!FILTER) {
                int id = 0;
                for (Double currentTime: myCompleteMap.get(current).getAllTimes()) {
                    str.append(prefix);
                    featureAggregations.append(prefix);

                    prefix = "\n";

                    featureAggregations.append(String.format("%d,%d,%.6f", getNumberOfDividers(current), id, currentTime));
                    str.append(String.format("%s,%d,%.6f", current, id++, currentTime));
                }
            }
        }

        if (SOURCE_MODEL.getNumberOfRuns() > 1 && !location.equals(NO_LOCATION)) {
            try {
                PrintWriter writer = new PrintWriter(location, "UTF-8");
                writer.print(CSV_HEADER);
                writer.print(str.toString());
                writer.close();
                writer = new PrintWriter(getFileNameWithSecondCSVExtension(new File(location)), "UTF-8");
                writer.print(CSV_HEADER);
                writer.print(featureAggregations.toString());
                writer.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    static public void add(BufferedReader bufReader, String location) throws IOException {
        HashMap<String, TimeContent> myCompleteMap = new HashMap<String, TimeContent>();

        String line;
        String splitter = " -> ";
        Boolean started = false;
        SOURCE_MODEL.getNumberOfRuns();

        PerformanceRun currentRun = null;
        ArrayList<String> currentTests = null;
        HashMap<String, TimeContent> currentMap = null;
        while ((line = bufReader.readLine()) != null) {
            if (started) {
                if (line.startsWith(MEASUREMENT_COUNTER)) {
                    HashMap<String, TimeContent> featureMap = new HashMap<String, TimeContent>();
                    currentMap = featureMap;
                    PerformanceRun model = new PerformanceRun();
                    currentRun = model;
                    SOURCE_MODEL.addPerformanceRun(currentRun);
                    currentRun.setNumberOfMeasurements(Integer.parseInt(line.split(MEASUREMENT_COUNTER)[1]));
                    if (currentTests != null) {
                        currentRun.setTests(currentTests);
                    }
                } else if (line.contains(splitter)) {
                    String[] featureAndTime = line.split(splitter);
                    String featureName = featureAndTime[0];
                    featureAndTime = featureAndTime[1].split(" ", 2);
                    Double time = Double.parseDouble(featureAndTime[0]);
                    featureAndTime = featureAndTime[1].split("ms, ", 2)[1].split(" ", 2);
                    Double outerTime = Double.parseDouble(featureAndTime[0]);
                    TimeContent timeContent = new TimeContent(featureAndTime[1], time, time, outerTime);
                    currentRun.getHashmap().put(featureName, timeContent);
                } else if (line.equals(HERCULES_END_MESSAGE)) {
                    started = false;
                    String[] keyArray = currentRun.getHashmap().keySet().toArray(new String[currentRun.getHashmap().size()]);
                    for (int i = 0; i < currentRun.getHashmap().size(); i++) {
                        double currentTime = currentRun.getHashmap().get(keyArray[i]).getOriginalTime();
                        for (int j = 0; j < currentRun.getHashmap().size(); j++) {
                            if (i != j && isSuccessor(keyArray[i], keyArray[j])) {
                                /*if (keyArray[i].equals(BASE_NAME)) {
                                    System.out.println("ERROR: " + currentTime + " <-> " + keyArray[j]);
                                }*/
                                currentTime -= currentRun.getHashmap().get(keyArray[j]).getOuterTime();
                                currentTime -= currentRun.getHashmap().get(keyArray[j]).getOriginalTime();
                            }
                        }
                        currentRun.getHashmap().get(keyArray[i]).updateFinalTime(currentTime);
                    }

                    /*Double total_time = 0.0;
                    for (String current : currentMap.keySet()) {
                        total_time += currentMap.get(current).getFinalTime();
                        if (!myCompleteMap.containsKey(current)) {
                            myCompleteMap.put(current, currentMap.get(current));
                        } else {
                            myCompleteMap.put(current, myCompleteMap.get(current).update(currentMap.get(current)));
                            //myCompleteMap.put(current, update(myCompleteMap.get(current), currentMap.get(current)));
                        }
                        if (location == NO_LOCATION) {
                            System.out.println(current + " -> " + currentMap.get(current));
                        }
                    }
                    if (PRINT_HASHMAP) {
                        System.out.println(String.format("Total time: %.6f", total_time));
                    }*/
                    currentRun = null;
                    currentTests = null;
                    currentMap = null;
                } else if (line.startsWith(TOTAL_TIME)) {
                    Scanner st = new Scanner(line);
                    while (!st.hasNextDouble()) {
                        st.next();
                    }
                    currentRun.setTotalTime(st.nextDouble());
                } else {
                    //System.out.println(line);
                }
            } else if (line.equals(HERCULES_START_MESSAGE)) {
                started = true;
            } else if (line.startsWith(MEASUREMENT_COUNTER)) {
                System.out.print(line.split("Performance counter: ")[1] + " ");
            } else if (line.startsWith(END)) {
                currentTests.add(line.split(END)[1]);
            } else if (line.startsWith(SQLITE_START)) {
                ArrayList<String> tests = new ArrayList<>();
                currentTests = tests;
            }
        }
    }

    private static HashMap sortByValues(HashMap map, Boolean ascending) {
        List list = new LinkedList(map.entrySet());
        if (ascending) {
            Collections.sort(list, new Comparator() {
                public int compare(Object o1, Object o2) {
                    return (((Double)((TimeContent)((Map.Entry) (o1)).getValue()).getFinalTime()).compareTo(((TimeContent)((Map.Entry) (o2)).getValue()).getFinalTime()));
                }
            });
        } else {
            Collections.sort(list, new Comparator() {
                public int compare(Object o1, Object o2) {
                    return (((Double)((TimeContent)((Map.Entry) (o2)).getValue()).getFinalTime()).compareTo(((TimeContent)((Map.Entry) (o1)).getValue()).getFinalTime()));
                }
            });
        }
        HashMap sortedHashMap = new LinkedHashMap();
        for (Iterator it = list.iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            sortedHashMap.put(entry.getKey(), entry.getValue());
        }
        return sortedHashMap;
    }

    static private int getNumberOfDividers(String string) {
        return string.replaceAll("[^#]", "").length();
    }

    static private boolean isSuccessor(String shortString, String possibleSuccessorString) {
        return (shortString.equals(BASE_NAME) && getNumberOfDividers(possibleSuccessorString) == 0) || (possibleSuccessorString.startsWith(shortString + "#") && getNumberOfDividers(possibleSuccessorString) == getNumberOfDividers(shortString) + 1);
    }

    static private boolean areInEpsilon(Double firstTime, Double secondTime) {
        return Math.abs(firstTime - secondTime) / ((firstTime + secondTime) / 2) < EPSILON;
    }
}
