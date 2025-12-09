using System;
using System.Data.SqlClient;

public static class Lab5
{
    public static void Run()
    {
        string connectionString = "Data Source=YOUR_SERVER;Initial Catalog=Bank;Integrated Security=True";

        using (SqlConnection conn = new SqlConnection(connectionString))
        {
            conn.Open();

            string insertQuery = @"
                INSERT INTO Customer (Account_no, Name, Address, DepositAmount) VALUES
                (1001, 'Sudan', 'Kathmandu', 1000),
                (1002, 'Ram', 'Lalitpur', 400),
                (1003, 'Hari', 'Bhaktapur', 800)";
            new SqlCommand(insertQuery, conn).ExecuteNonQuery();

            string deleteQuery = "DELETE FROM Customer WHERE DepositAmount < 500";
            int deleted = new SqlCommand(deleteQuery, conn).ExecuteNonQuery();

            Console.WriteLine($"Deleted {deleted} customer(s) with deposit < 500.");
        }

    }
}
