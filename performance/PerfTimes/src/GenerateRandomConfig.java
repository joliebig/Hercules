import java.io.*;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class GenerateRandomConfig {
    private final static String DISABLED = "-";
    private final static String ENABLED = "X";
    private final static String UNDECIDED = "?";
    private final static int SQLITE_ENABLE_FTS3 = 3;
    private final static int SQLITE_ENABLE_FTS4 = 4;
    private final static int NUMBER_OF_UNDECIDED_FEATURES = 23;
    private final static int NUMBER_OF_CONFIGS_TO_GENERATE = 50;

    /**
     * Generates random configurations and saves them as .csv file
     * @param configCsv
     */
    public static void generateConfigs(String configCsv) {
        Random r = new Random();
        Double border = Math.pow(2, NUMBER_OF_UNDECIDED_FEATURES);
        Set<String> allConfigurations = new HashSet<String>();

        for (int i = 0; i < NUMBER_OF_CONFIGS_TO_GENERATE; i++) {
            String currentRandom = fixConfiguration(Integer.toBinaryString(r.nextInt(border.intValue())));
            // Avoid duplicate configurations
            while (allConfigurations.contains(currentRandom)) {
                currentRandom = fixConfiguration(Integer.toBinaryString(r.nextInt(border.intValue())));
            }
            allConfigurations.add(currentRandom);
        }
        try (BufferedReader reader = new BufferedReader(new FileReader(new File(configCsv)))) {
            String line;
            int currentUndecidedFeature = 0;
            StringBuilder sb = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("Feature")) {
                    sb.append(line);
                    for (int i = 0; i < NUMBER_OF_CONFIGS_TO_GENERATE; i++) {
                        sb.append(i + ";");
                    }
                    sb.append("\n");
                } else {
                    String[] content = line.split(";");
                    String feature = content[0];
                    String status = content[1];
                    sb.append(feature + ";");
                    for (String currentConfiguration : allConfigurations) {
                        if (status.equals(UNDECIDED)) {
                            if (currentConfiguration.charAt(currentUndecidedFeature) == '0') {
                                sb.append(DISABLED + ";");
                            } else {
                                sb.append(ENABLED + ";");
                            }
                        } else {
                            sb.append(status + ";");
                        }
                    }
                    if (status.equals(UNDECIDED)) {
                        currentUndecidedFeature++;
                    }
                    sb.append("\n");
                }
            }
            //FileWriter fw = new FileWriter()
            //BufferedWriter writer = new BufferedWriter();

            System.out.println(sb.toString());
            FileWriter fwriter = new FileWriter(getExportCsvFile(configCsv));
            fwriter.append(sb);
            fwriter.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Alters given binary string configuration representation to account for an implication from the feature model.
     * Also adds zero padding.
     */
    private static String fixConfiguration(String currentConfiguration) {
        // Padding
        currentConfiguration = String.format("%" + NUMBER_OF_UNDECIDED_FEATURES + "s", currentConfiguration).replace(' ', '0');
        // SQLITE_ENABLE_FTS4 => SQLITE_ENABLE_FTS3
        if (currentConfiguration.charAt(SQLITE_ENABLE_FTS4) == '1') {
            return currentConfiguration.substring(0, SQLITE_ENABLE_FTS3) + '1' + currentConfiguration.substring(SQLITE_ENABLE_FTS4);
        }
        return currentConfiguration;
    }

    /**
     * Generates random configurations and saves them as .csv file
     * @param configCsv
     */
    public static void generateConfigs_old(String configCsv) {
        Random r = new Random();
        int[][] configurations = new int[NUMBER_OF_CONFIGS_TO_GENERATE][NUMBER_OF_UNDECIDED_FEATURES];
        for (int i = 0; i < configurations.length; i++) {
            configurations[i] = new int[]{r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2),
                    r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2),
                    r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2),
                    r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2), r.nextInt(2)};
            // SQLITE_ENABLE_FTS4 => SQLITE_ENABLE_FTS3
            if (configurations[i][SQLITE_ENABLE_FTS4] == 1) {
                configurations[i][SQLITE_ENABLE_FTS3] = 1;
            }
        }
        try (BufferedReader reader = new BufferedReader(new FileReader(new File(configCsv)))) {
            String line;
            int currentUndecidedFeature = 0;
            StringBuilder sb = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("Feature")) {
                    sb.append(line);
                    for (int i = 0; i < configurations.length; i++) {
                        sb.append(i + ";");
                    }
                    sb.append("\n");
                } else {
                    String[] content = line.split(";");
                    String feature = content[0];
                    String status = content[1];
                    sb.append(feature + ";");
                    for (int i = 0; i < configurations.length; i++) {
                        if (status.equals(UNDECIDED)) {
                            if (configurations[i][currentUndecidedFeature] == 0) {
                                sb.append(DISABLED + ";");
                            } else {
                                sb.append(ENABLED + ";");
                            }
                        } else {
                            sb.append(status + ";");
                        }
                    }
                    if (status.equals(UNDECIDED)) {
                        currentUndecidedFeature++;
                    }
                    sb.append("\n");
                }
            }
            //FileWriter fw = new FileWriter()
            //BufferedWriter writer = new BufferedWriter();

            System.out.println(sb.toString());
            FileWriter fwriter = new FileWriter(getExportCsvFile(configCsv));
            fwriter.append(sb);
            fwriter.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static File getExportCsvFile(String configCsv) {
        return new File(new File(configCsv).getParentFile() + File.separator + "random.ca2.csv");
    }
}
