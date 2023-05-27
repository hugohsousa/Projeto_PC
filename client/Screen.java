import processing.core.PApplet;

import java.io.IOException;

enum GameState {
    LoginMenu,
    Username,
    Password,
    Menu;

}

public class Screen extends PApplet implements Runnable {
    private final int width = 1280;
    private final int height = 720;
    private GameState state = GameState.LoginMenu;
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
            case LoginMenu:
                startLoginMenu();
                break;
            case Username:
                askUsername();
                break;
            case Password:
                askPassword();
                break;
            case Menu:
                startMenu();
        }
    }

    @Override
    public void keyPressed() {
        switch (this.state) {
            case LoginMenu:
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
                if (key == ENTER)
                    sendUserInfo();
                else if (key == BACKSPACE && login.passwordSize() != 0)
                    login.removeCharPassword();
                else if (key >= 'a' && key <= 'z')
                    login.addCharPassword(key);
                break;
        }
    }

    public void startLoginMenu() {
        text("1-Criar conta\n2-Entrar na conta", width/4, height/4);
    }

    private void startMenu() {
    }

    public void askUsername() {
        String message = "Username: ";
        text(message + login.getUsername(), width/2 - login.usernameSize()*4, height/32 + 4);
    }

    public void askPassword() {
        String message = "Password: ";
        text(message + "*".repeat(login.passwordSize()), width/2 - login.passwordSize()*4, 7*height/32 + 4);
    }

    public void sendUserInfo() {
        try {
            String message;
            if(login.isLoggedIn()) {
                cManager.send("login", login.getUsername() + '#' + login.getPassword());
                message = cManager.receive("login");
            } else {
                cManager.send("create_account", login.getUsername() + '#' + login.getPassword());
                message = cManager.receive("create_account");
            }
            processLoginInfo(message);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void reset() {
        this.state = GameState.LoginMenu;
        login.setUsername("");
        login.setPassword("");
        login.setLoggedIn(false);
    }
    public void processLoginInfo(String message) {
        if(message.equals("done"))
            this.state = GameState.Menu;
        else if(message.equals("erro 1")) {
            System.out.println("erro");
            reset();
        }
    }

    @Override
    public void run() {
        String[] processingArgs = {"Screen"};
        Screen screen = new Screen(cManager);
        PApplet.runSketch(processingArgs, screen);
    }
}
