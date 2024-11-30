import java.util.*;

public class Main {
    public static List<Integer> generateRandomList(int n, int patternLen) {
        Random random = new Random();
        List<Integer> nums = new ArrayList<>();

        // Hauptteil: Zufällige Zahlen mit gelegentlichen Mustern
        for (int i = 0; i < n; i++) {
            if (i % 50 == 0 && i + patternLen < n) {
                // Muster einfügen (z. B. alle 50 Zahlen)
                for (int j = 0; j < patternLen; j++) {
                    nums.add(random.nextInt(10)); // Ein zufälliges Muster erzeugen
                }
                i += patternLen - 1; // Überspringe die Länge des Musters
            } else {
                // Zufällige Zahl hinzufügen
                nums.add(random.nextInt(10));
            }
        }

        // Option: Cluster erstellen (lokale Ähnlichkeiten)
        for (int i = 0; i < n; i++) {
            if (random.nextDouble() < 0.2) { // 20% Wahrscheinlichkeit, Ähnlichkeit zu erzeugen
                nums.set(i, nums.get(i - 1) + random.nextInt(3) - 1); // Wert leicht variieren
                nums.set(i, Math.floorMod(nums.get(i), 10)); // Bereich sicherstellen
            }
        }

        return nums;
    }

    public static List<Integer> generateFullyRandomList(int n) {
        Random random = new Random();
        List<Integer> nums = new ArrayList<>();

        for(int i = 0; i < n; i++) {
            nums.add(random.nextInt(10));
        }

        return nums;
    }

    public static List<Integer> generateTrickList(int n, int patternLen) {
        Random random = new Random();
        List<Integer> nums = new ArrayList<>();
        List<Integer> pattern = new ArrayList<>();

        for(int i = 0; i < patternLen; i++) {
            pattern.add(random.nextInt(10));
        }

        for(int i = 0; i < n; i++) {
            if(i + patternLen < n && random.nextFloat() < 0.01) {
                for(int j = 0; j < patternLen; j++) {
                    nums.add(pattern.get(j));
                }
                i += patternLen - 1;
            }
            else{
                nums.add(random.nextInt(10));
            }
        }

        return nums;
    }

    // Methode zur Erkennung von wiederholten Mustern in einer nums
    public static Map<String, Integer> findTimeLoops(List<Integer> nums, int len) {
        Map<String, Integer> patternFrequency = new HashMap<>();

        for (int i = 0; i <= nums.size() - len; i++) {
            StringBuilder pattern = new StringBuilder();

            // Erstelle das Muster von der aktuellen Position
            for (int j = i; j < i + len; j++) {
                pattern.append(nums.get(j)).append(",");
            }

            String musterString = pattern.toString();

            // Erhöhe die Häufigkeit des gefundenen Musters
            patternFrequency.put(musterString, patternFrequency.getOrDefault(musterString, 0) + 1);
        }

        // Filtere nur Muster, die mehr als einmal vorkommen
        patternFrequency.entrySet().removeIf(entry -> entry.getValue() < 2);

        return patternFrequency;
    }

    public static void lookForOutliers(Map<String, Integer> timeLoops){
        double avg = 0.0;
        int max = 0;
        String maxKey = "";

        System.out.println("\nGefundene Zeitschleifen in timeLoops:");
        if (timeLoops.isEmpty()) {
            System.out.println("Keine Zeitschleifen gefunden.");
        } else {
            for (Map.Entry<String, Integer> entry : timeLoops.entrySet()) {
                System.out.println("Muster: " + entry.getKey() + " | Wiederholungen: " + entry.getValue());
            }
        }


        for(Map.Entry<String, Integer> entry : timeLoops.entrySet()) {
            avg += entry.getValue();
            if(entry.getValue() > max) {
                max = entry.getValue();
                maxKey = entry.getKey();
            }
        }
        avg = avg / timeLoops.size();

        if(max - avg > 3){
            System.out.println("\nVermeintliche Zeitschleife gefunden!");
            System.out.println(maxKey + " wurde " + max + " mal wiederholt.");
        }
        else{
            System.out.println("\nKeine vermeintliche Zeitschleife gefunden!");
        }

    }

    public static void main(String[] args) {
        // 1. Zufällige nums generieren
        List<Integer> randomNums = generateFullyRandomList(1000);
        List<Integer> trickNums = generateTrickList(1000, 5);

        // 2. Zeitschleifen erkennen
        int len = 5;
        Map<String, Integer> randomTimeLoops = findTimeLoops(randomNums, len);
        Map<String, Integer> trickTimeLoops = findTimeLoops(trickNums, len);

        lookForOutliers(randomTimeLoops);
        lookForOutliers(trickTimeLoops);
    }
}
