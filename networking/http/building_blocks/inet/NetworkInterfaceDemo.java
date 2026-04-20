// Import the necessary classes from the Java library.
// NetworkInterface is used to work with network interfaces (like Ethernet or Wi-Fi adapters).
// Enumeration is used to loop through a list of items (in this case, a list of network interfaces).
import java.net.NetworkInterface;
import java.util.Enumeration;

// Define a class named NetworkInterfaceDemo
public class NetworkInterfaceDemo {

    // The main method is the entry point of the program.
    // The `throws Exception` part means that the program might throw errors if something goes wrong (e.g., no network interfaces found).
    public static void main(String[] args) throws Exception {

        // Get a list of all network interfaces on the computer.
        // A network interface is like a network adapter (e.g., Wi-Fi, Ethernet, etc.).
        // NetworkInterface.getNetworkInterfaces() returns a list of all available network interfaces.
        Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();

        // Loop through each network interface in the list.
        // interfaces.hasMoreElements() checks if there are more network interfaces to process.
        while (interfaces.hasMoreElements()) {

            // Get the next network interface from the list.
            NetworkInterface ni = interfaces.nextElement();

            // Print the name of the network interface (e.g., "eth0" or "wlan0").
            // ni.getName() returns the name of the interface.
            System.out.println("Interface: " + ni.getName());

            // Print the MAC address of the network interface.
            // A MAC address is a unique identifier for a network adapter (e.g., "00:1A:2B:3C:4D:5E").
            // ni.getHardwareAddress() returns the MAC address as a byte array.
            // If the interface doesn't have a MAC address (e.g., a virtual interface), it will return null.
            byte[] macAddress = ni.getHardwareAddress();
            if (macAddress != null) {
                // Convert the byte array into a human-readable MAC address format.
                StringBuilder mac = new StringBuilder();
                for (byte b : macAddress) {
                    mac.append(String.format("%02X:", b)); // Convert each byte to a 2-digit hexadecimal value.
                }
                if (mac.length() > 0) {
                    mac.deleteCharAt(mac.length() - 1); // Remove the trailing ":" at the end.
                }
                System.out.println("MAC: " + mac.toString());
            } else {
                System.out.println("MAC: Not Available"); // If no MAC address is found.
            }
        }
    }
}