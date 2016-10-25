import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;

/**
 * Accumulates data from multiple performance runs
 */
public class Accumulator {
    private final static String sep = File.separator;
    private final static String HERCULES_START_MESSAGE = "-- Hercules Performance --";
    private final static String HERCULES_END_MESSAGE = "-- Hercules Performance End --";
    private final static String MEASUREMENT_COUNTER = "Measurement counter: ";
    private final static String TOTAL_TIME = "Total time: ";
    private final static String OVERHEAD = "Overhead: ";
    private final static String HASHMAP_SIZE = "Hashmap size: ";
    private final static String SPLITTER = " -> ";
    private static String RESULTDIR_MEDIAN = "median_results";
    private static String RESULTDIR_MEAN = "mean_results";
    private static Boolean useMedian = false;

    private static void addToHashMap(HashMap<String, ArrayList<Double>> hashmap, String key, double value) {
        if (hashmap.containsKey(key)) {
            hashmap.get(key).add(value);
        } else {
            hashmap.put(key, new ArrayList<Double>(Arrays.asList(value)));
        }
    }

    private static String removeLastChar(String string) {
        return string.substring(0, string.length() - 1);
    }

    private static Double computeMean(ArrayList<Double> list) {
        double currentSum = 0.0;
        for (Double value : list) {
            currentSum += value;
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

    private static String getComputedValue(ArrayList<Double> list) {
        if (useMedian) {
            return String.format("%.6f", computeMedian(list));
        } else {
            return String.format("%.6f", computeMean(list));
        }
    }

    private static void accumulate(File[] files) throws Exception {
        HashMap<String, ArrayList<Double>> resultValues = new HashMap();
        HashMap<String, ArrayList<Double>> resultOverhead = new HashMap();
        String prefix = "";
        String suffix = "";
        for (int i = 0; i < files.length; i++) {
            File currentFile = files[i];
            try (BufferedReader br = new BufferedReader(new FileReader(currentFile))) {
                String line;
                Boolean started = false;
                while ((line = br.readLine()) != null) {
                    if (started) {
                        if (currentFile == files[0] && line.equals(HERCULES_END_MESSAGE)) {
                            suffix += line + "\n";
                        } else if (line.startsWith(TOTAL_TIME)) {
                            Scanner st = new Scanner(line.replaceAll("[\\)\\()]",""));
                            while (!st.hasNextDouble()) {
                                st.next();
                            }
                            double totalTime = st.nextDouble();
                            while (!st.hasNextDouble()) {
                                st.next();
                            }
                            double totalOverhead = st.nextDouble();
                            addToHashMap(resultValues, removeLastChar(TOTAL_TIME), totalTime);
                            addToHashMap(resultValues, removeLastChar(OVERHEAD), totalOverhead);
                        } else if (line.startsWith(HASHMAP_SIZE)) {
                            addToHashMap(resultValues, removeLastChar(HASHMAP_SIZE), Double.parseDouble(line.split("Hashmap size: ")[1]));
                        } else  if (line.startsWith(MEASUREMENT_COUNTER)) {
                            addToHashMap(resultValues, removeLastChar(MEASUREMENT_COUNTER), Double.parseDouble(line.split("Measurement counter: ")[1]));
                        } else if (line.contains(SPLITTER)) {
                            String[] featureAndTime = line.split(SPLITTER);
                            String featureName = featureAndTime[0];
                            featureAndTime = featureAndTime[1].split(" ", 2);
                            Double time = Double.parseDouble(featureAndTime[0]);
                            featureAndTime = featureAndTime[1].split("ms, ", 2)[1].split(" ", 2);
                            Double outerTime = Double.parseDouble(featureAndTime[0]);
                            addToHashMap(resultValues, featureName, time);
                            addToHashMap(resultOverhead, featureName, outerTime);
                        }
                    } else {
                        if (line.equals(HERCULES_START_MESSAGE)) {
                            started = true;
                        }
                        if (currentFile == files[0]) {
                            prefix += line + "\n";
                        }
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        StringBuilder sb = new StringBuilder();
        sb.append(prefix);
        sb.append(HASHMAP_SIZE + (int) Math.round(computeMean(resultValues.get(removeLastChar(HASHMAP_SIZE)))) + "\n");
        sb.append(MEASUREMENT_COUNTER + (int) Math.round(computeMean(resultValues.get(removeLastChar(MEASUREMENT_COUNTER)))) + "\n");
        for (String key: resultValues.keySet()) {
            if (!isSpecialKey(key)) {
                sb.append(key + " -> " + getComputedValue(resultValues.get(key)) + " ms, " + getComputedValue(resultOverhead.get(key)) + " ms\n");
            }
        }
        sb.append(TOTAL_TIME + getComputedValue(resultValues.get(removeLastChar(TOTAL_TIME))) + " ms (overhead: " + getComputedValue(resultValues.get(removeLastChar(OVERHEAD))) + ")" + "\n");
        sb.append(suffix);
        String exportPath;
        if (useMedian) {
            exportPath = files[0].getAbsolutePath().replaceFirst("Run\\_[0-9]+", RESULTDIR_MEDIAN);
        } else {

            exportPath = files[0].getAbsolutePath().replaceFirst("Run\\_[0-9]+", RESULTDIR_MEAN);
        }
        new File(exportPath).getParentFile().mkdirs();
        PrintWriter writer = new PrintWriter(exportPath);
        writer.print(sb.toString());
        writer.close();
    }

    private static Boolean isSpecialKey(String key) {
        Boolean result = false;
        if (key.equals(removeLastChar(HASHMAP_SIZE))) {
            result = true;
        }
        if (key.equals(removeLastChar(MEASUREMENT_COUNTER))) {
            result = true;
        }
        if (key.equals(removeLastChar(TOTAL_TIME))) {
            result = true;
        }
        if (key.equals(removeLastChar(OVERHEAD))) {
            result = true;
        }
        return result;
    }

    public static void enableMedian() {
        useMedian = true;
    }

    public static void start(File directory) {
        if (directory.isDirectory()) {
            File[] folders = directory.listFiles(new FilenameFilter() {
                public boolean accept(File dir, String name) {
                    return name.startsWith("Run_");
                }
            });
            int currentCount = Integer.MIN_VALUE;

            for (File dir : folders) {
                if (currentCount == Integer.MIN_VALUE) {
                    currentCount = getFileCount(dir);
                } else if (currentCount != getFileCount(dir)) {
                    System.out.println("Filecount of " + dir + " does not match previous count of " + currentCount + " files.");
                    return;
                }
            }
            for (File innerDir : folders[0].listFiles()) {
                if (innerDir.isDirectory()) {
                    for (File performanceFile : innerDir.listFiles()) {
                        String perfFileName = performanceFile.getParentFile().getName() + sep + performanceFile.getName();
                        File[] currentPerfFiles = new File[folders.length];
                        for (int i = 0; i < folders.length; i++) {
                            currentPerfFiles[i] = new File(folders[i] + sep + perfFileName);
                        }
                        try {
                            accumulate(currentPerfFiles);
                        } catch (Exception e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        } else {
            System.out.println(directory + " is not a directory");
        }
    }

    public static int getFileCount(File file) {
        File[] files = file.listFiles();
        int count = 0;
        for (File f : files)
            if (f.isDirectory())
                count += getFileCount(f);
            else
                count++;

        return count;
    }
}
