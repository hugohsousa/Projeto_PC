import java.io.IOException;
import java.net.Socket;

public class ConnectionManager implements AutoCloseable {
    public static ConnectionManager start(Socket socket) throws IOException {
		return null;  }
    public void send(String type, String message) throws IOException {  }
    public String receive(String type) throws IOException {
		return type;  }
    public void close() throws IOException {  }
}
