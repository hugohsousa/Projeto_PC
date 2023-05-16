import java.net.Socket;

public class Client {
    public static void main(String[] args) {
        try  {
            if(args.length < 2) {
                System.exit(1);
            }

            Socket s = new Socket(args[0],Integer.parseInt(args[1]));

        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }
    }
}
