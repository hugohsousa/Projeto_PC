public class Login {
    private String username;
    private String password;

    private boolean loggedIn;

    Login() {
        this.username = "";
        this.password = "";
        this.loggedIn = false;
    }

    public String getUsername() {
        return this.username;
    }

    public String getPassword() {
        return this.password;
    }

    public boolean isLoggedIn() {
        return this.loggedIn;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setLoggedIn(boolean loggedIn) {
        this.loggedIn = loggedIn;
    }

    public void addCharUsername(char c) {
        this.username += c;
    }
    public void addCharPassword(char c) {
        this.password += c;
    }

    public void removeCharUsername() {
        this.username = this.username.substring(0, this.usernameSize() - 1);
    }
    public void removeCharPassword() {
        this.password = this.password.substring(0, this.passwordSize() - 1);
    }

    public int usernameSize() {
        return this.username.length();
    }
    public int passwordSize() {
        return this.password.length();
    }
}
