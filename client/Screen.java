import processing.core.PApplet;

enum GameState {
    Menu;
}

public class Screen extends PApplet implements Runnable {
    private final int width = 1600;
    private final int height = 800;
    Screen() {}

    @Override
    public void settings() {
        size(width,height);
    }

    @Override
    public void draw() {
        // Testing
        background(0);
        fill(255, 0, 0);
        ellipse(100, 100, 100, 100);
    }

    @Override
    public void run() {
        String[] processingArgs = {"Screen"};
        Screen screen = new Screen();
        PApplet.runSketch(processingArgs, screen);
    }
}
