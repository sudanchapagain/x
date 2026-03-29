package np.com.sudanchapagain.lab04;

import java.net.NetworkInterface;
import java.net.InetAddress;
import java.util.Enumeration;

public class Lab {
    public static void run() {
        try {
            Enumeration<NetworkInterface> nets = NetworkInterface.getNetworkInterfaces();

            while (nets.hasMoreElements()) {
                NetworkInterface nif = nets.nextElement();

                System.out.println("Interface: " + nif.getName() + " (" + nif.getDisplayName() + ")");
                byte[] mac = nif.getHardwareAddress();

                if (mac != null) {
                    StringBuilder sb = new StringBuilder();
                    for (int i = 0; i < mac.length; i++) sb.append(String.format(i == 0 ? "%02X" : "-%02X", mac[i]));
                    System.out.println("  MAC: " + sb.toString());
                }
                Enumeration<InetAddress> addrs = nif.getInetAddresses();

                while (addrs.hasMoreElements()) {
                    InetAddress ia = addrs.nextElement();
                    System.out.println("  Addr: " + ia.getHostAddress());
                }
            }
        } catch (Exception e) {
            System.out.println("Error listing interfaces: " + e.getMessage());
        }
    }
}
