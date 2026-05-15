package np.com.sudanchapagain;

import java.util.*;
import java.util.regex.*;

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String sql = "";

        while (true) {
//            print_banner();
            System.out.println("\nEnter Your Option");
            System.out.println("1. Enter SQL Query");
            System.out.println("2. Parse SQL Query");
            System.out.println("3. Show Raw SQL");
            System.out.println("4. Exit");
            System.out.print("Enter choice: ");

            int choice;
            try {
                choice = Integer.parseInt(scanner.nextLine().trim());
            } catch (NumberFormatException e) {
                choice = -1;
            }

            switch (choice) {
                case 1:
                    System.out.print("Enter SQL query: ");
                    sql = scanner.nextLine().trim();
                    break;
                case 2:
                    parseSQL(sql);
                    break;
                case 3:
                    System.out.println("\nYour query:\n" + sql);
                    break;
                case 4:
                    return;
                default:
                    System.out.println("Invalid choice.");
            }
        }
    }

    private static void parseSQL(String sql) {
        if (sql.isEmpty()) {
            System.out.println("No SQL entered.");
            return;
        }

        sql = sql.trim().replaceAll("\\s+", " ").toLowerCase(); // normalize spacing and case

        String selectPart = null;
        String fromPart = null;
        String wherePart = null;
        String orderPart = null;

        int selectIdx = sql.indexOf("select ");
        int fromIdx = sql.indexOf(" from ");
        if (selectIdx >= 0 && fromIdx > selectIdx) {
            selectPart = sql.substring(selectIdx + 7, fromIdx).trim();
        }

        int whereIdx = sql.indexOf(" where ");
        int orderIdx = sql.indexOf(" order by ");

        int i = orderIdx > 0 ? orderIdx : sql.length();
        if (fromIdx >= 0) {
            int endIdx = whereIdx > 0 ? whereIdx : i;
            fromPart = sql.substring(fromIdx + 6, endIdx).trim();
        }

        if (whereIdx > 0) {
            wherePart = sql.substring(whereIdx + 7, i).trim();
        }

        if (orderIdx > 0) {
            orderPart = sql.substring(orderIdx + 10).trim();
        }

        System.out.println("- Selected Columns: " + (selectPart == null ? "None" : selectPart));
        System.out.println("- Primary Table: " + (fromPart == null ? "None" : fromPart));
        System.out.println("- Filter Conditions: " + (wherePart == null ? "None" : wherePart));
        System.out.println("- Ordering Conditions: " + (orderPart == null ? "None" : orderPart));

        if (fromPart != null) {
            String[] parts = fromPart.split("\\s+join\\s+");
            System.out.println("- Joined Tables: " + (parts.length > 1 ? String.join(", ", java.util.Arrays.copyOfRange(parts, 1, parts.length)) : "None"));
        }
    }

//    public static void print_banner() {
//        System.out.println();
//        System.out.println("                                                                                               ");
//        System.out.println(" ▄█▀▀▀█▄█    ▄▄█▀▀██▄    ▀████▀        ▀███▀▀▀██▄                                              ");
//        System.out.println("▄██    ▀█  ▄██▀    ▀██▄    ██            ██   ▀██▄                                             ");
//        System.out.println("▀███▄      ██▀      ▀██    ██            ██   ▄██   ▄█▀██▄   ▀███▄███ ▄██▀███  ▄▄█▀██  ▀███▄███");
//        System.out.println("  ▀█████▄  ██        ██    ██            ███████   ██   ██     ██▀ ▀▀ ██   ▀▀ ▄█▀   ██   ██▀ ▀▀");
//        System.out.println("▄     ▀██  ██▄      ▄██    ██     ▄      ██         ▄█████     ██     ▀█████▄ ██▀▀▀▀▀▀   ██    ");
//        System.out.println("██     ██  ▀██▄    ▄██▀    ██    ▄█      ██        ██   ██     ██     █▄   ██ ██▄    ▄   ██    ");
//        System.out.println("█▀█████▀     ▀▀████▀▀    ██████████    ▄████▄      ▀████▀██▄ ▄████▄   ██████▀  ▀█████▀ ▄████▄  ");
//        System.out.println("                 ███                                                                           ");
//        System.out.println("                  ▀████▀                                                                       ");
//        System.out.println();
//    }

}
