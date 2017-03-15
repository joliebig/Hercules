import com.sun.org.apache.xpath.internal.SourceTree;
import expressionParser.FeatureModelParser;
import expressionParser.ScannerCreator;
import main.Pair;
import net.sf.javabdd.BDD;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.DecimalFormat;
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
    private final static String PERF_PREFIX = "perf_";
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
    private final static int SQLITE_MAX_INTERACTIONS = 6; // Currently a maximum interaction degree of 6 in SQLite
    private final static String TOTAL_TIME = "Total time: ";
    private final static DecimalFormat percentage = new DecimalFormat("####0.00");
    private static PerformanceModel SOURCE_MODEL = new PerformanceModel();
    private static Boolean EXPORT_AS_CSV = false;

    public static int getSqlitePredictionScenario() {
        return SQLITE_PREDICTION_SCENARIO;
    }

    public static void setSqlitePredictionScenario(int sqlitePredictionScenario) {
        SQLITE_PREDICTION_SCENARIO = sqlitePredictionScenario;
    }

    private static int SQLITE_PREDICTION_SCENARIO;

    public static String getInputMode() {
        return INPUT_MODE;
    }

    public static void setInputMode(String inputMode) {
        INPUT_MODE = inputMode;
    }

    public static String getPredictMode() {
        return PREDICT_MODE;
    }

    public static void setPredictMode(String predictMode) {
        PREDICT_MODE = predictMode;
    }

    private static String getCSVHeader() {
        return "id,InputMode,PredictMode,PercentageError,PercentageErrorInclVariance,VariancePercentage,MPTimePrediction,MPTimeResult,MPSharedFeatureDeviation,MPSharedFeatureDeviationInclVariance";
    }

    private static String INPUT_MODE;
    private static String PREDICT_MODE;

    public static class TimeContent implements Comparable<TimeContent> {
        private String additionalContent;
        private Double originalTime;
        private Double finalTime;
        private final Double outerTime;
        private List<Double> allTimes = new ArrayList<Double>();
        private Double deviation = 0.0;

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
                return String.format("%s -> %.6f ms", this.additionalContent, this.finalTime);
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
            if (this.allTimes.size() < 2) {
                return 0.0;
            }
            Double min = Collections.min(list);
            Double max = Collections.max(list);
            return Math.abs((max - min) * 100 / ((max + min) / 2));
        }

        /**
         * Calculates deviation between the lowest and highest value in given list.
         *
         * @param list
         * @return
         */
        private Double absoluteDeviation(List<Double> list) {
            if (this.allTimes.size() < 2) {
                return 0.0;
            }
            Double min = Collections.min(list);
            Double max = Collections.max(list);
            return max - min;
        }

        public Double getDeviation() {
            return deviation(this.allTimes);
        }

        public Double getAbsoluteDeviation() {
            return absoluteDeviation(this.allTimes);
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
        private int numberOfInteractions = 0;
        private ArrayList<String> tests = new ArrayList<>();
        private ArrayList<Double> distributions = new ArrayList<>();
        private double totalTime = 0.0;
        private String fileName = "";

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

        public void setFileName(String name) {
            this.fileName = name;
        }

        public String getMode() {
            return getModeForName(this.fileName);
        }

        public int getModeId() {
            return getModeIdForName(this.fileName);
        }

        public ArrayList<Double> getDistributions() {
            return this.distributions;
        }

        public void addToDistributions(Double newEntry) {
            this.distributions.add(newEntry);
        }

        public void setNumberOfInteractions(int interactions) {
            this.numberOfInteractions = interactions;
        }

        public int getNumberOfInteractions() {
            return this.numberOfInteractions;
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
        private class PredictionResult {
            public Double predictedTime;
            public Double predictedVariance;
            public Double variancePercentage;
            public Double resultTime;
            public Double percentageError;
            public Double percentageErrorWithVariance;
            public Double timeOnlyInPrediction;
            public Double timeOnlyInResult;
            public Double sharedFeatureDeviation;
            public Double sharedFeatureDeviationWithVariance;
            public TimeContent biggestDeviation;
            public Double biggestDeviationPercentageOfResultTime;
            public Double percentageDeviationInBiggestDeviationFeature;
            public Double absoluteDeviationInBiggestDeviationFeature;

            PredictionResult(Double predictedTime, Double predictedVariance, Double resultTime, Double timeOnlyInPrediction, Double timeOnlyInResult, Double sharedFeatureDeviation, Double sharedFeatureDeviationWithVariance, TimeContent biggestDeviation, Double percentageDeviationInBiggestDeviationFeature, Double absoluteDeviationInBiggestDeviationFeature) {
                this.predictedTime = predictedTime;
                this.predictedVariance = predictedVariance;
                this.variancePercentage = predictedVariance /predictedTime;
                this.resultTime = resultTime;
                this.percentageError = percentageError(predictedTime, resultTime);
                this.percentageErrorWithVariance = percentageErrorWithVariance(predictedTime, predictedVariance, resultTime);
                this.timeOnlyInPrediction = timeOnlyInPrediction;
                this.timeOnlyInResult = timeOnlyInResult;
                this.sharedFeatureDeviation = sharedFeatureDeviation;
                this.sharedFeatureDeviationWithVariance = sharedFeatureDeviationWithVariance;
                this.biggestDeviation = biggestDeviation;
                this.absoluteDeviationInBiggestDeviationFeature = absoluteDeviationInBiggestDeviationFeature;
                this.biggestDeviationPercentageOfResultTime = absoluteDeviationInBiggestDeviationFeature / resultTime;
                this.percentageDeviationInBiggestDeviationFeature = percentageDeviationInBiggestDeviationFeature;
            }

            PredictionResult(Double predictedTime, Double predictedVariance, Double resultTime, Double timeOnlyInPrediction, Double timeOnlyInResult, Double sharedFeatureDeviation) {
                this.predictedTime = predictedTime;
                this.predictedVariance = predictedVariance;
                this.resultTime = resultTime;
                this.percentageError = percentageError(predictedTime, resultTime);
                this.timeOnlyInPrediction = timeOnlyInPrediction;
                this.timeOnlyInResult = timeOnlyInResult;
                this.sharedFeatureDeviation = sharedFeatureDeviation;
            }

            public String toString() {
                StringBuilder sb = new StringBuilder();
                sb.append(String.format("Predicted: %.6f ms ± %.6f ms\n", predictedTime, predictedVariance));
                sb.append(String.format("Actual result: %.6f ms\n", resultTime));
                sb.append(String.format("Percentage error: %s\n", percentageToString(percentageError)));
                sb.append(String.format("Percentage error incl variance: %s\n", percentageToString(percentageErrorWithVariance)));
                sb.append(String.format("Variance percentage: %s\n", percentageToString(variancePercentage)));
                sb.append(String.format("Time only in prediction: %.6f ms [%s of result time]\n", timeOnlyInPrediction, percentageToString(timeOnlyInPrediction / resultTime)));
                sb.append(String.format("Time only in result: %.6f ms [%s of result time]\n", timeOnlyInResult, percentageToString(timeOnlyInResult / resultTime)));
                sb.append(String.format("Time deviation in shared features: %.6f ms [%s of result time]\n", sharedFeatureDeviation, percentageToString(sharedFeatureDeviation / resultTime)));
                sb.append(String.format("Time deviation in shared features incl variance: %.6f ms [%s of result time]\n", sharedFeatureDeviationWithVariance, percentageToString(sharedFeatureDeviationWithVariance / resultTime)));

                return sb.toString();
            }

            public String toCsvString() {
                StringBuilder sb = new StringBuilder();
                sb.append(String.format(",%.6f", percentageError));

                return sb.toString();
            }
        }
        private ArrayList<PerformanceRun> performanceRuns = new ArrayList<>();
        private ArrayList<PredictionResult> predictionResults = new ArrayList<>();
        private PerformanceRun targetRun = null;
        private HashMap<String, TimeContent> hashmap = new HashMap<>();
        private BDD configuration;
        private Double predictedTime = Double.MIN_VALUE;
        private Double predictedVariance = Double.MIN_VALUE;
        HashMap<String, TimeContent> featuresValidInConfiguration = new HashMap<>();

        public String printPrediction() {
            if (predictionResults.size() == 1) {
                PredictionResult lonelyResult = predictionResults.get(0);
                StringBuilder sb = new StringBuilder();
                Double meanPercentageError = Math.abs(lonelyResult.percentageError);
                Double meanPercentageErrorWithVariance = Math.abs(lonelyResult.percentageErrorWithVariance);
                Double variancePercentage = lonelyResult.variancePercentage;
                Double meanTimeOnlyInPrediction = lonelyResult.timeOnlyInPrediction / lonelyResult.resultTime;
                Double meanTimeOnlyInResult = lonelyResult.timeOnlyInResult / lonelyResult.resultTime;
                Double meanSharedFeatureDeviation = lonelyResult.sharedFeatureDeviation / lonelyResult.resultTime;
                Double meanSharedFeatureDeviationWithVariance = lonelyResult.sharedFeatureDeviationWithVariance / lonelyResult.resultTime;
                sb.append(String.format("Absolute mean percentage error: %s\n", percentageToString(meanPercentageError)));
                sb.append(String.format("Absolute mean percentage error incl variance: %s\n", percentageToString(meanPercentageErrorWithVariance)));
                sb.append(String.format("Variance percentage: %s\n", percentageToString(variancePercentage)));
                sb.append(String.format("Mean percentage of time only in prediction: %s\n", percentageToString(meanTimeOnlyInPrediction)));
                sb.append(String.format("Mean percentage of time only in result: %s\n", percentageToString(meanTimeOnlyInResult)));
                sb.append(String.format("Mean percentage of shared feature deviation: %s\n", percentageToString(meanSharedFeatureDeviation)));
                sb.append(String.format("Mean percentage of shared feature deviation incl variance: %s\n", percentageToString(meanSharedFeatureDeviationWithVariance)));

                if (EXPORT_AS_CSV) {
                    /*StringBuilder csvSB = new StringBuilder();
                    csvSB.append(getSqlitePredictionScenario() + "," + getInputMode() + "," + getPredictMode() + ",");
                    csvSB.append(absMeanPercentage + "," + absMeanPercentageWithVariance + "," + variancePercentage + "," + percentageTimeInPrediction + "," + percentageTimeInResult + "," + percentageSharedFeatureDeviation + "," + percentageSharedFeatureDeviationWithVariance);
                    exportResult(csvSB.toString());*/

                    //exportResult(String.format("%d,%s,%s,%s,%s,%s,%s,%s,%s,%s", getSqlitePredictionScenario(), getInputMode(), getPredictMode(), percentageToString(meanPercentageError), percentageToString(meanPercentageErrorWithVariance), percentageToString(meanVariancePercentage), percentageToString(meanTimeOnlyInPrediction), percentageToString(meanTimeOnlyInResult), percentageToString(meanSharedFeatureDeviation), percentageToString(meanSharedFeatureDeviationWithVariance)));
                    exportResult(String.format("%d,%s,%s,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f", getSqlitePredictionScenario(), getInputMode(), getPredictMode(), meanPercentageError, meanPercentageErrorWithVariance, variancePercentage, meanTimeOnlyInPrediction, meanTimeOnlyInResult, meanSharedFeatureDeviation, meanSharedFeatureDeviationWithVariance));
                }

                return sb.toString();
                //return predictionResults.get(0).toString();
            } else {
                ArrayList<Double> predictedTimes = new ArrayList<>();
                ArrayList<Double> predictedVariance = new ArrayList<>();
                ArrayList<Double> resultTime = new ArrayList<>();
                ArrayList<Double> percentageError = new ArrayList<>();
                ArrayList<Double> percentageErrorWithVariance = new ArrayList<>();
                ArrayList<Double> variancePercentage = new ArrayList<>();
                ArrayList<Double> timeOnlyInPrediction = new ArrayList<>();
                ArrayList<Double> timeOnlyInResult = new ArrayList<>();
                ArrayList<Double> sharedFeatureDeviation = new ArrayList<>();
                ArrayList<Double> sharedFeatureDeviationWithVariance = new ArrayList<>();

                for (PredictionResult result: predictionResults) {
                    predictedTimes.add(result.predictedTime);
                    predictedVariance.add(result.predictedVariance);
                    resultTime.add(result.resultTime);
                    percentageError.add(result.percentageError);
                    percentageErrorWithVariance.add(result.percentageErrorWithVariance);
                    variancePercentage.add(result.variancePercentage);
                    timeOnlyInPrediction.add(result.timeOnlyInPrediction / result.resultTime);
                    timeOnlyInResult.add(result.timeOnlyInResult / result.resultTime);
                    sharedFeatureDeviation.add(result.sharedFeatureDeviation / result.resultTime);
                    sharedFeatureDeviationWithVariance.add(result.sharedFeatureDeviationWithVariance / result.resultTime);
                }
                //PredictionResult averages = new PredictionResult(computeMean(predictedTimes), computeMean(predictedVariance), computeMean(resultTime), computeMean(timeOnlyInPrediction), computeMean(timeOnlyInResult), computeMean(sharedFeatureDeviation));
                //return averages.toString();
                Double meanPercentageError = computeAbsoluteMean(percentageError);
                Double meanPercentageErrorWithVariance = computeAbsoluteMean(percentageErrorWithVariance);
                Double meanTimeOnlyInPrediction = computeAbsoluteMean(timeOnlyInPrediction);
                Double meanTimeOnlyInResult = computeAbsoluteMean(timeOnlyInResult);
                Double meanSharedFeatureDeviation =  computeAbsoluteMean(sharedFeatureDeviation);
                Double meanSharedFeatureDeviationWithVariance =  computeAbsoluteMean(sharedFeatureDeviationWithVariance);
                Double meanVariancePercentage = computeMean(variancePercentage);
                StringBuilder sb = new StringBuilder();
                //sb.append(String.format("Mean predicted time: %.6f ms ± %.6f ms\n", computeMean(predictedTimes), computeMean(predictedVariance)));
                //sb.append(String.format("Mean result time: %.6f ms\n", computeMean(resultTime)));
                sb.append(String.format("Absolute mean percentage error: %s\n", percentageToString(meanPercentageError)));
                sb.append(String.format("Absolute mean percentage error incl variance: %s\n", percentageToString(meanPercentageErrorWithVariance)));
                sb.append(String.format("Variance percentage: %s\n", percentageToString(meanVariancePercentage)));
                sb.append(String.format("Mean percentage of time only in prediction: %s\n", percentageToString(meanTimeOnlyInPrediction)));
                sb.append(String.format("Mean percentage of time only in result: %s\n", percentageToString(meanTimeOnlyInResult)));
                sb.append(String.format("Mean percentage of shared feature deviation: %s\n", percentageToString(meanSharedFeatureDeviation)));
                sb.append(String.format("Mean percentage of shared feature deviation incl variance: %s\n", percentageToString(meanSharedFeatureDeviationWithVariance)));

                if (EXPORT_AS_CSV) {
                    //exportResult(String.format("%d,%s,%s,%s,%s,%s,%s,%s,%s,%s", getSqlitePredictionScenario(), getInputMode(), getPredictMode(), percentageToString(meanPercentageError), percentageToString(meanPercentageErrorWithVariance), percentageToString(meanVariancePercentage), percentageToString(meanTimeOnlyInPrediction), percentageToString(meanTimeOnlyInResult), percentageToString(meanSharedFeatureDeviation), percentageToString(meanSharedFeatureDeviationWithVariance)));
                    exportResult(String.format("%d,%s,%s,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f,%.6f", getSqlitePredictionScenario(), getInputMode(), getPredictMode(), meanPercentageError, meanPercentageErrorWithVariance, meanVariancePercentage, meanTimeOnlyInPrediction, meanTimeOnlyInResult, meanSharedFeatureDeviation, meanSharedFeatureDeviationWithVariance));
                }

                return sb.toString();
            }
        }

        private void resetValues() {
            this.featuresValidInConfiguration = new HashMap<>();
            this.predictedTime = Double.MIN_VALUE;
            this.predictedVariance = Double.MIN_VALUE;
        }

        public int getNumberOfRuns() {
            return this.performanceRuns.size();
        }

        public String printPredictedTime() {
            if (getPredictedVariance() != 0.0) {
                return String.format("Predicted: %.6f ms ± %.6f ms", getPredictedTime(), getPredictedVariance());
            } else {
                return String.format("Predicted: %.6f ms", getPredictedTime());
            }
        }

        /**
         * Calculates the accuracy of the prediction.
         *
         * @param predictedTime
         * @param actualTime
         * @return
         */
        private Double percentageError(Double predictedTime, Double actualTime) {
            return (predictedTime - actualTime) / actualTime;
        }

        /**
         * Calculates the accuracy of the prediction taking variance into account.
         *
         * @param predictedTime
         * @param actualTime
         * @return
         */
        private Double percentageErrorWithVariance(Double predictedTime, Double variance, Double actualTime) {
            Double variancePercentage = variance / actualTime;
            Double percentageError = (predictedTime - actualTime) / actualTime;
            if (predictedTime < actualTime && predictedTime + variance < actualTime) {
                return (predictedTime + variance - actualTime) / actualTime;
            } else if (predictedTime > actualTime && predictedTime - variance > actualTime){
                return (predictedTime - variance - actualTime) / actualTime;
            } else {
                return 0.0;
            }
        }

        private String percentageErrorString(Double predictedTime, Double actualTime) {
            return percentage.format(percentageError(predictedTime, actualTime) * 100) + "%";
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

        public void setTargetRun(File file) {
            try {
                setTargetRun(readToHashMap(file));
            } catch (Exception e) {
                e.printStackTrace();
            }
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
                    this.featuresValidInConfiguration.put(current, hashmap.get(current));
                    this.predictedTime += hashmap.get(current).getFinalTime();
                    this.predictedVariance += hashmap.get(current).getVariance();
                }
            }
        }

        public Double getResultTime() {
            if (this.targetRun == null) {
                return Double.MIN_VALUE;
            }
            return this.targetRun.getTotalTime();
        }

        public String printResultTime() {
            if (this.targetRun == null) {
                return "";
            }
            return String.format("Actual result: %.6f ms", this.getResultTime());
        }

        private String percentageOfResult(Double value) {
            return percentageToString(value / this.getResultTime());
        }

        public void comparePredictionToResult() {
            if (this.targetRun == null) {
                return;
            }
            ArrayList<TimeContent> featuresOnlyInResult = new ArrayList<TimeContent>();
            ArrayList<TimeContent> featuresOnlyInPrediction = new ArrayList<TimeContent>();
            ArrayList<TimeContent> sharedFeatures = new ArrayList<TimeContent>();
            ArrayList<TimeContent> sharedFeaturesWithVariance = new ArrayList<TimeContent>();
            Set<String> allKeys = new HashSet<String>();
            allKeys.addAll(this.getHashmap().keySet());
            allKeys.addAll(targetRun.getHashmap().keySet());

            for (String key : allKeys) {
                if (this.getHashmap().containsKey(key) && targetRun.getHashmap().containsKey(key)) {
                    Double difference = this.getHashmap().get(key).getFinalTime() - targetRun.getHashmap().get(key).getFinalTime();
                    Double differenceWithVariance = computeDifferenceWithVariance(this.getHashmap().get(key).getFinalTime(), this.getHashmap().get(key).getVariance(), targetRun.getHashmap().get(key).getFinalTime());
                    if ((difference >= 0.0 && difference < differenceWithVariance) || (difference < 0 && difference > differenceWithVariance)) {
                        System.out.println("wtf");
                    }
                    //System.out.println(String.format("Diff: %.6f\t\t Var: %.6f", difference, differenceWithVariance));
                    sharedFeatures.add(new TimeContent(key, 0.0, difference, 0.0));
                    sharedFeaturesWithVariance.add(new TimeContent(key, 0.0, differenceWithVariance, 0.0));
                } else if (!this.getHashmap().containsKey(key) && targetRun.getHashmap().containsKey(key)) {
                    featuresOnlyInPrediction.add(targetRun.getHashmap().get(key));
                } else if (this.featuresValidInConfiguration.containsKey(key) && !targetRun.getHashmap().containsKey(key)) {
                    featuresOnlyInResult.add(SOURCE_MODEL.getHashmap().get(key));
                }
            }

            Double timeOnlyInPrediction = 0.0;
            for (TimeContent current : featuresOnlyInPrediction) {
                timeOnlyInPrediction += current.getFinalTime();
            }

            Double timeOnlyInResult = 0.0;
            for (TimeContent current : featuresOnlyInResult) {
                timeOnlyInResult += current.getFinalTime();
            }

            Double sharedFeatureDeviation = 0.0;
            for (TimeContent current : sharedFeatures) {
                sharedFeatureDeviation += current.getFinalTime();
            }

            Double sharedFeatureDeviationWithVariance = 0.0;
            for (TimeContent current : sharedFeaturesWithVariance) {
                sharedFeatureDeviationWithVariance += current.getFinalTime();
            }

            /*System.out.println(this.printResultTime());
            System.out.println("Percentage error: " + percentageErrorString(this.getResultTime(), this.getPredictedTime()));
            System.out.println("Time only in prediction: " + timeOnlyInPrediction + " [" + percentageOfResult(timeOnlyInPrediction) + " of result time]");
            System.out.println("Time only in result: " + timeOnlyInResult + " [" + percentageOfResult(timeOnlyInResult) + " of result time]");
            System.out.println("Time deviation in shared features: " + sharedFeatureDeviation + " [" + percentageOfResult(sharedFeatureDeviation) + " of result time]");*/
            TimeContent biggestDeviation;
            TimeContent deviationMin = Collections.min(sharedFeatures);
            TimeContent deviationMax = Collections.max(sharedFeatures);
            if (this.getPredictedTime() > this.getResultTime()) {
                biggestDeviation = deviationMax;
                //System.out.println("Biggest deviation in feature: " + Collections.max(sharedFeatures) + " [deviation: " + percentage.format(this.getHashmap().get(Collections.max(sharedFeatures).additionalContent).getDeviation()) + "%]");
            } else {
                biggestDeviation = deviationMin;
                //System.out.println("Biggest deviation in feature: " + Collections.min(sharedFeatures) + " [deviation: " + percentage.format(this.getHashmap().get(Collections.min(sharedFeatures).additionalContent).getDeviation()) + "%]");
            }
            /*System.out.println("Biggest deviation in feature: " + biggestDeviation + " [percentage deviation measured: " + percentage.format(this.getHashmap().get(biggestDeviation.additionalContent).getDeviation()) + "%, percentage (max-min) of result time " + percentageOfResult(this.getHashmap().get(biggestDeviation.additionalContent).getAbsoluteDeviation()) + "]");
            System.out.println("Feature deviation vs result: " + percentage.format(this.getHashmap().get(biggestDeviation.additionalContent).getAbsoluteDeviation() * 100 / getResultTime()) + "%");
            System.out.println();*/
            PredictionResult currentResult = new PredictionResult(this.getPredictedTime(), this.getPredictedVariance(), this.getResultTime(), timeOnlyInPrediction, timeOnlyInResult, sharedFeatureDeviation, sharedFeatureDeviationWithVariance, biggestDeviation, this.getHashmap().get(biggestDeviation.additionalContent).getDeviation(), this.getHashmap().get(biggestDeviation.additionalContent).getAbsoluteDeviation());
            System.out.println(currentResult);
            predictionResults.add(currentResult);
        }

        public void comparePredictionToResult(File resultFile) {
            this.setTargetRun(resultFile);
            this.comparePredictionToResult();
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

        private void generateAndExportDistributions() {
            StringBuilder sb = new StringBuilder();
            int totalMaxInteractions = SQLITE_MAX_INTERACTIONS;
            for (PerformanceRun perfRun: this.performanceRuns) {
                HashMap<Integer, Double> interactionTimes = new HashMap<>();
                Double totalTime = perfRun.getTotalTime();
                perfRun.addToDistributions(totalTime);
                Double baseTime = perfRun.getHashmap().get(BASE_NAME).getFinalTime();
                perfRun.addToDistributions(baseTime);
                int maxInteractions = 0;
                for (String current: perfRun.getHashmap().keySet()) {
                    if (!current.equals(BASE_NAME)) {
                        Double currentTime = perfRun.getHashmap().get(current).getFinalTime();
                        int interactions = current.length() - current.replace("#", "").length();
                        if (interactions > maxInteractions) {
                            if (interactions > totalMaxInteractions) {
                                System.out.println("Warning: New maximum interaction degree: " + interactions);
                            }
                            maxInteractions = interactions;
                        }
                        if (interactionTimes.containsKey(interactions)) {
                            Double newValue = interactionTimes.get(interactions) + currentTime;
                            interactionTimes.put(interactions, newValue);
                        } else {
                            interactionTimes.put(interactions, currentTime);
                        }
                    }
                }
                perfRun.setNumberOfInteractions(maxInteractions);
                for (int i = 0; i <= maxInteractions; i++) {
                    perfRun.addToDistributions(interactionTimes.get(i));
                }
            }
            String header = "id,Mode,ModeID,MaxInteractionDegree,TotalTime,Interaction,Time";
            for (PerformanceRun perfRun: this.performanceRuns) {
                Double totalTime = perfRun.getDistributions().get(0);
                for (int i = 1; i < perfRun.getDistributions().size(); i++) {
                    String interaction = String.valueOf(i - 2);
                    sb.append(String.format("%d,%s,%d,%d,%.6f,%s,%.6f\n", getSqlitePredictionScenario(), perfRun.getMode(),
                            perfRun.getModeId(), perfRun.getNumberOfInteractions(), totalTime, interaction, perfRun.getDistributions().get(i)));
                }
            }
            File resultFile = new File("distributions.csv");
            BufferedWriter bw = null;

            try {
                if (!resultFile.exists()) {
                    bw = new BufferedWriter(new FileWriter("distributions.csv", false));
                    bw.write(header);
                    bw.newLine();
                    bw.flush();
                    bw.close();
                }
                bw = new BufferedWriter(new FileWriter("distributions.csv", true));
                bw.write(sb.toString());
                bw.flush();
            } catch (IOException e) {
                e.printStackTrace();
            } finally {
                if (bw != null) try {
                    bw.close();
                } catch (IOException e2) {

                }
            }
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
        System.out.println(perfModel.printPredictedTime());
        return currentPredictedTime;
    }

    static private Double predict(PerformanceModel perfModel, BDD configuration, File resultFile) {
        perfModel.setBDD(configuration);
        perfModel.setTargetRun(resultFile);
        Double currentPredictedTime = perfModel.getPredictedTime();
        //System.out.println(perfModel.printPredictedTime());
        perfModel.comparePredictionToResult();
        return currentPredictedTime;
    }

    static public void createFeatureDistribution(File location) {
        for (File child : location.listFiles()) {
            if (child.getName().startsWith(PERF_PREFIX)) {
                try (BufferedReader reader = new BufferedReader(new FileReader(child))) {
                    add(reader, child.getName());
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }
        SOURCE_MODEL.generateAndExportDistributions();
    }

    static public void predict(String predictMode, String structLocation, File location) {
        switch (predictMode) {
            case "allyes":
                if (new File(structLocation).isDirectory()) {
                    BDD configuration = getBDDfromConfigFile(structLocation + "/allyes/allyes_include.h");
                    File resultFile = new File(getResultFile(location, new File(NO_LOCATION), predictMode));
                    if (resultFile.exists()) {
                        predict(SOURCE_MODEL, configuration, resultFile);
                    } else {
                        predict(SOURCE_MODEL, configuration);
                    }
                    System.out.println("Averages:\n" + SOURCE_MODEL.printPrediction());
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
                            File resultFile = new File(getResultFile(location, child, predictMode));
                            System.out.println(child.getName());
                            if (resultFile.exists()) {
                                predict(SOURCE_MODEL, configuration, resultFile);
                            } else {
                                predict(SOURCE_MODEL, configuration);
                            }
                        }
                    }
                    System.out.println("Averages:\n" + SOURCE_MODEL.printPrediction());
                }
                break;
            default:
                break;
        }
    }

    public static String getResultTime(File file) {
        String secondToLastLine = tail2(file, 2).split(System.lineSeparator(), 2)[0];
        Scanner st = new Scanner(secondToLastLine);
        while (!st.hasNextDouble()) {
            st.next();
        }
        Double actualTime = st.nextDouble();
        return String.format("Actual result: %.6f ms", actualTime);
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

    static public PerformanceRun readToHashMap(File file) throws IOException {
        String line;
        String splitter = " -> ";
        Boolean started = false;
        PerformanceRun currentRun = new PerformanceRun();

        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            while ((line = reader.readLine()) != null) {
                if (started) {
                    if (line.startsWith(MEASUREMENT_COUNTER)) {
                        currentRun.setNumberOfMeasurements(Integer.parseInt(line.split(MEASUREMENT_COUNTER)[1]));
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
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return currentRun;
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

    static public String percentageToString(Double percent) {
        return percentage.format(percent * 100) + "%";
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
                break;
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
                break;
            case "pairwise":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_PAIRWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_PAIRWISE + id + ".txt";
                break;
            case "random":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_FEATUREWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_RANDOM + id + ".txt";
                break;
            case "allyes":
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_NAME_ALLYES;
                break;
            case "codecoverage":
                id = configFile.getName().replaceAll(CONFIG_PREFIX_FEATUREWISE, "").replaceAll("\\D+","");
                resultFile = location.getAbsolutePath() + File.separator + PERF_FILE_PREFIX_CODECOVERAGE + id + ".txt";
                break;
            default:
                resultFile = "";
                break;
        }
        return resultFile;
    }

    static public String getModeForName(String fileName) {
        if (fileName.startsWith(PERF_FILE_PREFIX_FEATUREWISE)) {
            return "featurewise";
        } else if (fileName.startsWith(PERF_FILE_PREFIX_PAIRWISE)) {
            return "pairwise";
        } else if (fileName.startsWith(PERF_FILE_PREFIX_RANDOM)) {
            return "random";
        } else if (fileName.startsWith(PERF_FILE_PREFIX_CODECOVERAGE)) {
            return "codecoverage";
        } else if (fileName.equals(PERF_FILE_NAME_ALLYES)) {
            return "allyes";
        } else {
            return null;
        }
    }

    static public int getModeIdForName(String fileName) {
        if (fileName.startsWith(PERF_FILE_PREFIX_FEATUREWISE)) {
            String prefixtrimmed = fileName.replaceFirst("^" + PERF_FILE_PREFIX_FEATUREWISE, "");
            String suffixtrimmed = prefixtrimmed.substring(0, prefixtrimmed.lastIndexOf("."));
            return Integer.parseInt(suffixtrimmed);
        } else if (fileName.startsWith(PERF_FILE_PREFIX_PAIRWISE)) {
            String prefixtrimmed = fileName.replaceFirst("^" + PERF_FILE_PREFIX_PAIRWISE, "");
            String suffixtrimmed = prefixtrimmed.substring(0, prefixtrimmed.lastIndexOf("."));
            return Integer.parseInt(suffixtrimmed);
        } else if (fileName.startsWith(PERF_FILE_PREFIX_RANDOM)) {
            String prefixtrimmed = fileName.replaceFirst("^" + PERF_FILE_PREFIX_RANDOM, "");
            String suffixtrimmed = prefixtrimmed.substring(0, prefixtrimmed.lastIndexOf("."));
            return Integer.parseInt(suffixtrimmed);
        } else if (fileName.startsWith(PERF_FILE_PREFIX_CODECOVERAGE)) {
            String prefixtrimmed = fileName.replaceFirst("^" + PERF_FILE_PREFIX_CODECOVERAGE, "");
            String suffixtrimmed = prefixtrimmed.substring(0, prefixtrimmed.lastIndexOf("."));
            return Integer.parseInt(suffixtrimmed);
        } else if (fileName.equals(PERF_FILE_NAME_ALLYES)) {
            return 0;
        } else {
            return 0;
        }
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

    static public void add(BufferedReader bufReader, String fileName) throws IOException {
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
                    currentRun.setFileName(fileName);
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

    private static Double computeMean(ArrayList<Double> list) {
        double currentSum = 0.0;
        for (Double value : list) {
            currentSum += value;
        }
        return currentSum / list.size();
    }

    private static Double computeAbsoluteMean(ArrayList<Double> list) {
        double currentSum = 0.0;
        for (Double value : list) {
            currentSum += Math.abs(value);
        }
        return currentSum / list.size();
    }

    private static Double computeMedian(ArrayList<Double> list) {
        Double[] array = list.toArray(new Double[list.size()]);
        Arrays.sort(array);
        if (array.length%2 == 1) {
            return array[array.length/2];
        } else {
            return (array[array.length/2 - 1] + array[array.length/2]) / 2.0;
        }
    }

    private static Double computeDifferenceWithVariance(Double predictedTime, Double predictedVariance, Double actualTime) {
        if (predictedTime > actualTime && predictedTime - predictedVariance > actualTime) {
            return predictedTime - predictedVariance - actualTime;
        } else if (predictedTime < actualTime && predictedTime + predictedVariance < actualTime) {
            return predictedTime + predictedVariance -actualTime;
        } else {
            return 0.0;
        }
    }

    private static void exportResult(String resultString) {
        File resultFile = new File("results.csv");
        BufferedWriter bw = null;

        try {
            if (!resultFile.exists()) {
                bw = new BufferedWriter(new FileWriter("results.csv", false));
                bw.write(getCSVHeader());
                bw.newLine();
                bw.flush();
                bw.close();
            }
            bw = new BufferedWriter(new FileWriter("results.csv", true));
            bw.write(resultString);
            bw.newLine();
            bw.flush();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (bw != null) try {
                bw.close();
            } catch (IOException e2) {

            }
        }
    }

    public void exportAsCsv(Boolean export) {
        this.EXPORT_AS_CSV = export;
    }
}
