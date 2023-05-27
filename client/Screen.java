import processing.core.PApplet;

import java.io.IOException;

enum GameState {
    LoginMenu,
    Username,
    Password,
    Wait,
    Menu,
    Join,
    Leaderboard,
    Logout,
    RemoveAccount,
    Game;
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
        switch (this.state) {
            case LoginMenu:
                background(0);
                startLoginMenu();
                break;
            case Username:
                background(0);
                askUsername();
                break;
            case Password:
                background(0);
                askPassword();
                break;
            case Menu:
                background(0);
                startMenu();
                break;
            case Join:
                joinGame();
                break;
            case Leaderboard:
                break;
            case Logout:
                sendLogout();
                break;
            case RemoveAccount:
                reqRemoveAccount();
                break;
            case Game:
                drawGame();
                break;
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
                if (key == ENTER) {
                    this.state = GameState.Wait;
                    sendUserInfo();
                }
                else if (key == BACKSPACE && login.passwordSize() != 0)
                    login.removeCharPassword();
                else if (key >= 'a' && key <= 'z')
                    login.addCharPassword(key);
                break;
            case Menu:
                if(key == '1')
                    this.state = GameState.Join;
                if(key == '2')
                    this.state = GameState.Leaderboard;
                if(key == '3')
                    this.state = GameState.Logout;
                if(key == '4')
                    this.state = GameState.RemoveAccount;
                break;
            case Game:
                if(key == 'a')
                    sendMovInfo("left");
                if(key == 'd')
                    sendMovInfo("right");
                if(key == 'w')
                    sendMovInfo("forward");
                break;
        }
    }


    public void startLoginMenu() {
        text("1-Criar conta\n2-Entrar na conta", width/2 - 6, height/2 - 1);
    }

    private void startMenu() {
        text("1-Play\n2-Leaderboard\n3-Logout\n4-Remover Conta", width/2 - 7*4, height/2 - 16);
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
        this.state = GameState.Menu;
        if(message.equals("done"))
            this.state = GameState.Menu;
        else if(message.equals("erro 1")) {
            System.out.println("erro");
            reset();
        }
    }

    private void sendLogout() {
        try {
            cManager.send("logout","");
            if(cManager.receive("logout").equals("done")) {
                reset();
                this.state = GameState.LoginMenu;
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void reqRemoveAccount() {
       try {
            cManager.send("delete_account","");
            String message = cManager.receive("delete_account");
            if (cManager.receive("delete_account").equals("done")) {
                reset();
            }
       } catch (IOException e) {
            throw new RuntimeException(e);
       }
    }
    public void joinGame() {
        try {
            cManager.send("join","");
            String message = cManager.receive("start");
            if (cManager.receive("start").equals("inicio?"))
                this.state = GameState.Game;
            else
                reset();
            // To Do - Decide o que fazer se existir erro
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void sendMovInfo(String move) {
        try {
            cManager.send("move",move);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    public void drawGame() {
        background(0);

    }

    @Override
    public void run() {
        String[] processingArgs = {"Screen"};
        Screen screen = new Screen(cManager);
        PApplet.runSketch(processingArgs, screen);
    }
}
