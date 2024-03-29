import processing.core.PApplet;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import static java.lang.Thread.sleep;

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
    Game,
    Playing,
    Over;
}

public class Screen extends PApplet implements Runnable {
    private Lock lock = new ReentrantLock();
    private final int width = 1280;
    private final int height = 720;
    private GameState state = GameState.LoginMenu;
    // ConnectionManager
    ConnectionManager cManager;
    // Login
    private Login login = new Login();
    private ArrayList<Piece> pieces = new ArrayList<Piece>();
    private String error = "";
    private boolean left;
    private boolean up;
    private boolean right;

    Screen(ConnectionManager cManager) {
        this.cManager = cManager;
        this.left = false;
        this.right = false;
        this.up = false;
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
                new Thread(() -> {
                    receiveGameInfo();
                }).start();
                state = GameState.Playing;
                break;
            case Playing:
                drawGame();
                break;
            case Over:
                reset();
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
            case Playing:
                if(key == 'a')
                    this.left = true;
                if(key == 'd')
                    this.right = true;
                if(key == 'w')
                    this.up = true;
                sendMovInfo();
                break;
        }
    }

    @Override
    public void keyReleased() {
        switch (this.state) {
            case Playing:
                if(key == 'a')
                    this.left = false;
                if(key == 'd')
                    this.right = false;
                if(key == 'w')
                    this.up = false;
                break;
        }
    }

    public void startLoginMenu() {
        text("1-Entrar na conta\n2-Criar conta", width/2 - 6, height/2 - 1);
        text(error, width / 2 - error.length()/2 , height/16);
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
        if(message.equals("invalid_password")) {
            System.out.println("Invalid Password");
            reset();
            error = "Invalid Password";
        } else if (message.equals("invalid_username")) {
            System.out.println("Invalid Username");
            reset();
            error = "Invalid Username";
        } else if (message.equals("user_exists")) {
            System.out.println("User already in database");
            reset();
            error = "Username is taken";
        } else if(login.isLoggedIn()) {
            if(message.equals("done")) {
                this.state = GameState.Menu;
            }
        } else {
            if(message.equals("done")) {
                this.state = GameState.LoginMenu;
                reset();
                error = "Account Created Successfully";
            }
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
                error = "Account Removed";
            }
       } catch (IOException e) {
            throw new RuntimeException(e);
       }
    }
    public void joinGame() {
        try {
            cManager.send("join",login.getUsername());

            if (cManager.receive("join").equals("done")) {
                System.out.println("Received Game\n");
                this.state = GameState.Game;
            } else {
                reset();
            }
            // To Do - Decide o que fazer se existir erro
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private void sendMovInfo() {
        String toSend = "";
        try {
            if (this.up)
                toSend = toSend + "up";
            if (this.left) {
                if(toSend.length() > 0) {
                    toSend = toSend + "#";
                }
                toSend = toSend + "left";
            }
            if (this.right) {
                if(toSend.length() > 0) {
                    toSend = toSend + "#";
                }
                toSend = toSend + "right";
            }
            if(toSend.length() > 0) {
                cManager.send("move", toSend);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
    public synchronized void receiveGameInfo() {
        String message;
        try {
           message = cManager.receive("game");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        if(message.equals("win")) {
            System.out.println("Ganhou\n");
            state = GameState.Over;
            error = "Ganhou\n";
        } else if(message.equals("lose")) {
                System.out.println("Perdeu\n");
                state = GameState.Over;
                error = "Perdeu\n";
        } else {
            lock.lock();
            try {
                pieces.clear();
                background(100);
                String[] gameInfo = message.split("#");
                for (String info : gameInfo) {
                    pieces.add(new Piece(info.split(","), login.getUsername()));
                }
            } finally {
                lock.unlock();
            }
        }
        receiveGameInfo();
    }

    public void drawGame() {
        lock.lock();
        try {
            background(100);
            for (Piece piece : this.pieces) {
                if (piece.getId() == 1 || piece.getId() == 2) {
                    fill(piece.getR(), piece.getG(), piece.getB());
                    pushMatrix();
                    translate(piece.getX(), piece.getY());
                    circle(0, 0, piece.getSize());
                    rotate(piece.getViewAngle());
                    rect(0, 0, 30, 1);
                    popMatrix();
                } else {
                    fill(piece.getR(), piece.getG(), piece.getB());
                    pushMatrix();
                    translate(piece.getX(), piece.getY());
                    circle(0, 0, piece.getSize());
                    popMatrix();
                }
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void run() {
        String[] processingArgs = {"Screen"};
        Screen screen = new Screen(cManager);
        PApplet.runSketch(processingArgs, screen);
    }
}
