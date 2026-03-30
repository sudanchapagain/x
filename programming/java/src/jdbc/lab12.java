import java.sql.*;
import java.util.Scanner;

public class lab12 {
    static final String URL = "jdbc:mysql://localhost:3306/lab_db";
    static final String USER = "root";
    static final String PASS = "password";

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        try (Connection conn = DriverManager.getConnection(URL, USER, PASS)) {
            String sql = "INSERT INTO employees (id, name, address, salary) " + "VALUES (?, ?, ?, ?)";
            PreparedStatement ps = conn.prepareStatement(sql);

            while (true) {
                System.out.print("Enter ID: ");
                int id = Integer.parseInt(sc.nextLine());

                System.out.print("Enter Name: ");
                String name = sc.nextLine();

                System.out.print("Enter Address: ");
                String address = sc.nextLine();

                System.out.print("Enter Salary: ");
                double salary = Double.parseDouble(sc.nextLine());

                ps.setInt(1, id);
                ps.setString(2, name);
                ps.setString(3, address);
                ps.setDouble(4, salary);

                int rows = ps.executeUpdate();
                if (rows > 0) System.out.println("Record inserted successfully.");

                System.out.print("Do you want to add another record? (yes/no): ");
                String ans = sc.nextLine();
                if (!ans.equalsIgnoreCase("yes")) break;
            }

        } catch (SQLException e) {
            System.out.println("Database error: " + e.getMessage());
        }
        sc.close();
    }
}
