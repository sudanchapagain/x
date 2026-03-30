import java.sql.*;
import java.util.Scanner;

public class lab11 {
    static final String URL = "jdbc:mysql://localhost:3306/lab_db";
    static final String USER = "root";
    static final String PASS = "";

    static Scanner sc = new Scanner(System.in);

    public static void insertRecord() {
        try (Connection conn = DriverManager.getConnection(URL, USER, PASS); Statement stmt = conn.createStatement()) {
            System.out.print("Enter ID: ");
            int id = Integer.parseInt(sc.nextLine());
            System.out.print("Enter Name: ");
            String name = sc.nextLine();
            System.out.print("Enter Address: ");
            String address = sc.nextLine();
            System.out.print("Enter Salary: ");
            double salary = Double.parseDouble(sc.nextLine());

            String sql = "INSERT INTO employees (id, name, address, salary) VALUES (" + id + ", '" + name + "', '" + address + "', " + salary + ")";
            int rows = stmt.executeUpdate(sql);
            if (rows > 0) System.out.println("Record inserted successfully.");
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    public static void displayAll() {
        try (Connection conn = DriverManager.getConnection(URL, USER, PASS); Statement stmt = conn.createStatement()) {
            String sql = "SELECT * FROM employees";
            ResultSet rs = stmt.executeQuery(sql);

            System.out.println("ID\tName\tAddress\tSalary");
            while (rs.next()) {
                System.out.println(rs.getInt("id") + "\t" + rs.getString("name") + "\t" + rs.getString("address") + "\t" + rs.getDouble("salary"));
            }
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    public static void displayById() {
        try (Connection conn = DriverManager.getConnection(URL, USER, PASS); Statement stmt = conn.createStatement()) {
            System.out.print("Enter ID: ");
            int id = Integer.parseInt(sc.nextLine());

            String sql = "SELECT * FROM employees WHERE id=" + id;
            ResultSet rs = stmt.executeQuery(sql);

            if (rs.next()) {
                System.out.println("ID: " + rs.getInt("id"));
                System.out.println("Name: " + rs.getString("name"));
                System.out.println("Address: " + rs.getString("address"));
                System.out.println("Salary: " + rs.getDouble("salary"));
            } else {
                System.out.println("Record not found.");
            }
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    public static void updateRecord() {
        try (Connection conn = DriverManager.getConnection(URL, USER, PASS); Statement stmt = conn.createStatement()) {
            System.out.print("Enter ID to update: ");
            int id = Integer.parseInt(sc.nextLine());
            System.out.print("Enter field to update (name/address/salary): ");
            String field = sc.nextLine();
            System.out.print("Enter new value: ");
            String value = sc.nextLine();

            String sql;
            if (field.equalsIgnoreCase("salary")) {
                sql = "UPDATE employees SET " + field + "=" + value + " WHERE id=" + id;
            } else {
                sql = "UPDATE employees SET " + field + "='" + value + "' WHERE id=" + id;
            }

            int rows = stmt.executeUpdate(sql);
            if (rows > 0) System.out.println("Record updated successfully.");
            else System.out.println("Record not found.");
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    public static void deleteRecord() {
        try (Connection conn = DriverManager.getConnection(URL, USER, PASS); Statement stmt = conn.createStatement()) {
            System.out.print("Enter ID to delete: ");
            int id = Integer.parseInt(sc.nextLine());

            String sql = "DELETE FROM employees WHERE id=" + id;
            int rows = stmt.executeUpdate(sql);
            if (rows > 0) System.out.println("Record deleted successfully.");
            else System.out.println("Record not found.");
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        while (true) {
            System.out.println("\n1. Insert Record");
            System.out.println("2. Display All Records");
            System.out.println("3. Display Record by ID");
            System.out.println("4. Update Record");
            System.out.println("5. Delete Record");
            System.out.println("6. Exit");
            System.out.print("Enter choice: ");
            int choice = Integer.parseInt(sc.nextLine());

            switch (choice) {
                case 1:
                    insertRecord();
                    break;
                case 2:
                    displayAll();
                    break;
                case 3:
                    displayById();
                    break;
                case 4:
                    updateRecord();
                    break;
                case 5:
                    deleteRecord();
                    break;
                case 6:
                    System.exit(0);
                default:
                    System.out.println("Invalid choice");
            }
        }
    }
}
