import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class ConnectionManager implements AutoCloseable {

    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;

    ConnectionManager(Socket s) throws IOException {
        this.socket = s;
        this.in = new BufferedReader(new InputStreamReader(s.getInputStream()));
        this.out = new PrintWriter(s.getOutputStream());
    }

    public static ConnectionManager start(Socket s) throws IOException {
        return new ConnectionManager(s);
    }

    public void send(String type, String message) throws IOException {
        out.println(type + ':' + message);
        out.flush();
    }

    public String receive(String type) throws IOException {
        String message = in.readLine();
        while(message == null) {
            message = in.readLine();
        }

        String[] splitMessage = message.split(":",2);
        while(!splitMessage[0].equals(type)) {
            message = in.readLine();
            splitMessage = message.split(":",2);
        }
        return splitMessage[1];
    }

    public void close() throws IOException {
    }
}