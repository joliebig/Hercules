import com.sun.deploy.util.StringUtils;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

public class perftimes {
    private final static String baseName = "BASE";
    private final static String herculesStartMessage = "-- Hercules Performance --";
    private final static String herculesEndMessage = "-- Hercules Performance End --";
    private final static int epsilon = 10;

    public static class TimeContent {
        private String additionalContent;
        private Double originalTime;
        private Double finalTime;
        private final Double outerTime;
        private List<Double> allTimes = new ArrayList<Double>();

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

        public double getFinalTime() {
            if (this.allTimes.size() > 1) {
                return average(this.allTimes);
            } else {
                return this.finalTime;
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
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
                execute(reader);
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else if (args.length == 1) {
            File file = new File(args[0]);
            if (file.exists() && !file.isDirectory()) {
                try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                    execute(reader);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            } else {
                System.out.println("could not find file at path: " + args[0]);
            }
        } else {
            System.out.println("too many arguments");
        }
    }

    static private void execute(BufferedReader bufReader) throws IOException {
        HashMap<String, TimeContent> myCompleteMap = new HashMap<String, TimeContent>();
        HashMap<String, TimeContent> myMap = new HashMap<String, TimeContent>();

        String line;
        String splitter = " -> ";
        Boolean started = false;
        int runCounter = 0;
        while ((line = bufReader.readLine()) != null) {
            if (started) {
                if (line.contains(splitter)) {
                    String[] featureAndTime = line.split(splitter);
                    String featureName = featureAndTime[0];
                    featureAndTime = featureAndTime[1].split(" ", 2);
                    Double time = Double.parseDouble(featureAndTime[0]);
                    featureAndTime = featureAndTime[1].split("ms, ", 2)[1].split(" ", 2);
                    Double outerTime = Double.parseDouble(featureAndTime[0]);
                    TimeContent timeContent = new TimeContent(featureAndTime[1], time, time, outerTime);
                    myMap.put(featureName, timeContent);
                } else if (line.equals(herculesEndMessage)) {
                    started = false;
                    String[] keyArray = myMap.keySet().toArray(new String[myMap.size()]);
                    for (int i = 0; i < myMap.size(); i++) {
                        double currentTime = myMap.get(keyArray[i]).getOriginalTime();
                        for (int j = 0; j < myMap.size(); j++) {
                            if (i != j && isSuccessor(keyArray[i], keyArray[j])) {
                                /*if (keyArray[i].equals(baseName)) {
                                    System.out.println("ERROR: " + currentTime + " <-> " + keyArray[j]);
                                }*/
                                currentTime -= myMap.get(keyArray[j]).getOuterTime();
                                currentTime -= myMap.get(keyArray[j]).getOriginalTime();
                            }
                        }
                        myMap.get(keyArray[i]).updateFinalTime(currentTime);
                    }
                    for (String current: myMap.keySet()) {
                        if (!myCompleteMap.containsKey(current)) {
                            myCompleteMap.put(current, myMap.get(current));
                        } else {
                            myCompleteMap.put(current, myCompleteMap.get(current).update(myMap.get(current)));
                            //myCompleteMap.put(current, update(myCompleteMap.get(current), myMap.get(current)));
                        }
                        //System.out.println(current + " -> " + myMap.get(current));
                    }
                    myMap.clear();
                } else {
                    //System.out.println(line);
                }
            } else if (line.equals(herculesStartMessage)) {
                runCounter++;
                started = true;
            } else if (line.startsWith("Performance counter: ")) {
                System.out.print(line.split("Performance counter: ")[1] + " ");
            }
        }
        if (runCounter > 1) {
            System.out.println("Number of runs: " + runCounter);
        }
        for (String current: myCompleteMap.keySet()) {
            System.out.println(current + " -> " + myCompleteMap.get(current));
        }
    }

    static private int getNumberOfDividers(String string) {
        return string.replaceAll("[^#]", "").length();
    }

    static private boolean isSuccessor(String shortString, String possibleSuccessorString) {
        return (shortString.equals(baseName) && getNumberOfDividers(possibleSuccessorString) == 0) || (possibleSuccessorString.startsWith(shortString + "#") && getNumberOfDividers(possibleSuccessorString) == getNumberOfDividers(shortString) + 1);
    }

    static private boolean areInEpsilon(Double firstTime, Double secondTime) {
        return Math.abs(firstTime - secondTime) / ((firstTime + secondTime) / 2) < epsilon;
    }
}
