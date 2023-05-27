import processing.core.PApplet;

import java.io.IOException;

enum GameState {
    Menu,
    Username,
    Password;
}

public class Screen extends PApplet implements Runnable {
    private final int width = 1280;
    private final int height = 720;
    private GameState state = GameState.Menu;
    // ConnectionManager
    ConnectionManager cManager;
    // Login
    private Login login = new Login();
    
    Screen(ConnectionManager cManager) {
        this.cManager = cManager;
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
                    login.setLoggedIn(true);
                }
                if(key == '2') {
                    this.state = GameState.Username;
                }
                break;
            case Username:
                if (key == ENTER) {
                    this.state = GameState.Password;
                }
                else if (key == BACKSPACE && login.usernameSize() != 0)
                    login.removeCharUsername();
                else if (key >= 'a' && key <= 'z')
                    login.addCharUsername(key);
                break;
            case Password:
                if (key == ENTER) {
                    try {
                        sendUserInfo();
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
                else if (key == BACKSPACE && login.passwordSize() != 0)
                    login.removeCharPassword();
                else if (key >= 'a' && key <= 'z')
                    login.addCharPassword(key);
                break;
        }
    }

    private void startMenu() {
        text("1-Criar conta\n2-Entrar na conta", width/4, height/4);
    }

    private void askUsername() {
        String message = "Username: ";
        text(message + login.getUsername(), width/2 - login.usernameSize()*4, height/32 + 4);
    }

    private void askPassword() {
        String message = "Password: ";
        text(message + "*".repeat(login.passwordSize()), width/2 - login.passwordSize()*4, 7*height/32 + 4);
    }

    private void sendUserInfo() throws IOException {
        if(login.isLoggedIn()) {
            cManager.send("login", login.getUsername() + '#' + login.getPassword());
        } else {
            cManager.send("create_account", login.getUsername() + '#' + login.getPassword());
        }
    }

    @Override
    public void run() {
        String[] processingArgs = {"Screen"};
        Screen screen = new Screen(cManager);
        PApplet.runSketch(processingArgs, screen);
    }
}
