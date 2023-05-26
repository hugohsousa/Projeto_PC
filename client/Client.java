import java.net.Socket;

public class Client {
    public static void main(String[] args) {
        try  {
            if(args.length < 2) {
                // Ignore while testing
                //System.exit(1);
            }

            //Socket s = new Socket(args[0],Integer.parseInt(args[1]));

            new Thread(new Screen()).start();

        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }
    }
}