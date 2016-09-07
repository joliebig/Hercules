import java.io.*;
import java.util.Arrays;
import java.util.Random;

public class GenerateRandomConfig {
    private final static String DISABLED = "-";
    private final static String ENABLED = "X";
    private final static String UNDECIDED = "?";
    private final static int SQLITE_ENABLE_FTS3 = 3;
    private final static int SQLITE_ENABLE_FTS4 = 4;
    private final static int NUMBER_OF_UNDECIDED_FEATURES = 23;
    private final static int NUMBER_OF_CONFIGS_TO_GENERATE = 10;

    public static void generateConfigs(String configCsv) {
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
