// Import the InetAddress class from the java.net package.
// This class is used to work with IP addresses and hostnames.
import java.net.InetAddress;

// Define a class named InetAddressDemo
public class InetAddressDemo {

    // The main method is the entry point of the program
    public static void main(String[] args) throws Exception {
        
        // Use the InetAddress class to get information about a host (in this case, "google.com")
        // getByName() is a static method that takes a hostname (or IP address) and returns an InetAddress object.
        InetAddress addr = InetAddress.getByName("google.com");

        // Print the host name associated with the InetAddress object.
        // getHostName() returns the hostname (e.g., "google.com").
        System.out.println("Host Name: " + addr.getHostName());

        // Print the IP address associated with the InetAddress object.
        // getHostAddress() returns the IP address as a string (e.g., "142.250.190.14").
        System.out.println("IP Address: " + addr.getHostAddress());

        // Check if the host is reachable within a specified timeout (in milliseconds).
        // isReachable() sends an ICMP ping request to the host and returns true if the host responds within the timeout.
        // Here, the timeout is set to 2000 milliseconds (2 seconds).
        System.out.println("Reachable: " + addr.isReachable(2000));
    }
}