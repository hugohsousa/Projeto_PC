import processing.core.PApplet;

enum GameState {
    Menu,
    Username,
    Password;
}

public class Screen extends PApplet implements Runnable {
    private final int width = 1280;
    private final int height = 720;
    private GameState state = GameState.Menu;
    // To-do Decide if we need to create a new class
    private String username = "";
    private String password = "";
    
    Screen() {
    }

    @Override
    public void settings() {
        size(width,height);
    }

    @Override
    public void draw() {
        // Testing
        background(0);
        switch (this.state) {
            case Menu:
                startMenu();
                break;
            case Username:
                askUsername();
                break;
            case Password:
                askPassword();
                break;
        }
    }

    @Override
    public void keyPressed() {
        switch (this.state) {
            case Menu:
                if(key == '1') {
                    this.state = GameState.Username;
                }
                if(key == '2') {
                    this.state = GameState.Username;
                }
                break;
            case Username:
                if (key == ENTER) {
                    // Enviar o login
                    System.out.println(this.username);
                    this.state = GameState.Password;
                }
                else if (key == BACKSPACE && this.username.length() != 0)
                    this.username = this.username.substring(0, this.username.length() - 1);
                else if (key >= 'a' && key <= 'z')
                    this.username += key;
                break;
            case Password:
                if (key == ENTER) {
                    // Enviar o login
                    System.out.println(this.password);
                }
                else if (key == BACKSPACE && this.password.length() != 0)
                    this.password = this.password.substring(0, this.password.length() - 1);
                else if (key >= 'a' && key <= 'z')
                    this.password += key;
                break;
        }
    }

    private void startMenu() {
        text("1-Criar conta\n2-Entrar na conta", width/4, height/4);
    }

    private void askUsername() {
        String message = "Username: ";
        text(message + username, width/2 - username.length()*4, height/32 + 4);
    }

    private void askPassword() {
        String message = "Password: ";
        text(message + "*".repeat(password.length()), width/2 - username.length()*4, 7*height/32 + 4);
    }
    
    @Override
    public void run() {
        String[] processingArgs = {"Screen"};
        Screen screen = new Screen();
        PApplet.runSketch(processingArgs, screen);
    }
}
