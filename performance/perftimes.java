import java.io.*;
import java.util.*;
import java.nio.file.Path;

public class perftimes {
    private final static String BASE_NAME = "BASE";
    private final static String HERCULES_START_MESSAGE = "-- Hercules Performance --";
    private final static String HERCULES_END_MESSAGE = "-- Hercules Performance End --";
    private final static int EPSILON = 10;
    private final static Boolean ASCENDING = true;
    private final static Boolean DESCENDING = false;
    private final static Boolean FILTER = true;
    private final static Double FILTER_THRESHOLD = 0.0005;
    private final static String NO_LOCATION = "";

    public static class TimeContent implements Comparable<TimeContent> {
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

        @Override
        public int compareTo(TimeContent other) {
            return Double.compare(this.getFinalTime(), other.getFinalTime());
        }
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            try (BufferedReader reader = new BufferedReader(new InputStreamReader(System.in))) {
                execute(reader, NO_LOCATION);
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else if (args.length == 1) {
            File file = new File(args[0]);
            if (file.exists() && !file.isDirectory()) {
                try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                    execute(reader, getFileNameWithCSVExtension(file));
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

    static private void execute(BufferedReader bufReader, String location) throws IOException {
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
                } else if (line.equals(HERCULES_END_MESSAGE)) {
                    started = false;
                    String[] keyArray = myMap.keySet().toArray(new String[myMap.size()]);
                    for (int i = 0; i < myMap.size(); i++) {
                        double currentTime = myMap.get(keyArray[i]).getOriginalTime();
                        for (int j = 0; j < myMap.size(); j++) {
                            if (i != j && isSuccessor(keyArray[i], keyArray[j])) {
                                /*if (keyArray[i].equals(BASE_NAME)) {
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
            } else if (line.equals(HERCULES_START_MESSAGE)) {
                runCounter++;
                started = true;
            } else if (line.startsWith("Performance counter: ")) {
                System.out.print(line.split("Performance counter: ")[1] + " ");
            }
        }
        if (runCounter > 1) {
            System.out.println("Number of runs: " + runCounter);
        }
        myCompleteMap = sortByValues(myCompleteMap, DESCENDING);

        StringBuilder str = new StringBuilder();
        String prefix = "";
        for (String current: myCompleteMap.keySet()) {
            if (FILTER && myCompleteMap.get(current).getFinalTime() > FILTER_THRESHOLD) {
                int id = 0;
                for (Double currentTime: myCompleteMap.get(current).getAllTimes()) {
                    str.append(prefix);
                    prefix = "\n";
                    str.append(String.format("%s,%d,%.6f", current, id++, currentTime));
                }
                System.out.println(current + " -> " + myCompleteMap.get(current));
            } else if (!FILTER) {
                int id = 0;
                for (Double currentTime: myCompleteMap.get(current).getAllTimes()) {
                    str.append(prefix);
                    prefix = "\n";
                    str.append(String.format("%s,%d,%.6f", current, id++, currentTime));
                }
                System.out.println(current + " -> " + myCompleteMap.get(current));
            }
        }

        if (runCounter > 1 && !location.equals(NO_LOCATION)) {
            PrintWriter writer = new PrintWriter(location, "UTF-8");
            writer.print(str.toString());
            writer.close();
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
