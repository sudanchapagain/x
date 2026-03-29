package np.com.sudanchapagain.lab10;

import java.net.*;
import java.util.*;
import java.io.IOException;

public class Lab {
    public static class RememberingProxySelector extends ProxySelector {
        public final List<URI> seen = new ArrayList<>();

        @Override
        public List<Proxy> select(URI uri) {
            seen.add(uri);
            return Collections.singletonList(Proxy.NO_PROXY);
        }

        @Override
        public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
        }
    }

    public static void run() {
        RememberingProxySelector ps = new RememberingProxySelector();
        ProxySelector.setDefault(ps);

        try {
            new URL("http://example.com/").openConnection().connect();
        } catch (Exception ignored) {
        }
        System.out.println("Seen URIs: " + ps.seen);
    }
}
