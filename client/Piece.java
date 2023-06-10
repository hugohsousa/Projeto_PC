import processing.core.PVector;

public class Piece {
    private int id;
    private float x;
    private float y;
    private int size;

    private int r;
    private int g;
    private int b;
    public Piece(String[] piece) {
        this.id = Integer.parseInt(piece[0]);
        setColor(piece[1]);
        this.x = Float.parseFloat(piece[2]);
        this.y = Float.parseFloat(piece[3]);
        System.out.println("Debug:Id: " + this.id + " X: " + this.x + " Y: " + this.y);
    }

    public void setColor(String color) {
        if(color.equals("white")) {
            this.r = 255;
            this.g = 255;
            this.b = 255;
            this.size = 50;
        } else if (color.equals("black")) {
            this.r = 0;
            this.g = 0;
            this.b = 0;
            this.size = 50;
        } else if(color.equals("red")) {
            this.r = 255;
            this.g = 0;
            this.b = 0;
            this.size = 20;
        } else if(color.equals("green")) {
            this.r = 0;
            this.g = 255;
            this.b = 0;
            this.size = 20;
        } else if(color.equals("blue")) {
            this.r = 0;
            this.g = 0;
            this.b = 255;
            this.size = 20;
        }
    }
    public float getX() {
        return this.x;
    }

    public float getY() {
        return this.y;
    }

    public int getR() {
        return this.r;
    }

    public int getG() {
        return this.g;
    }

    public int getB() {
        return this.b;
    }

    public float getSize() {
        return this.size;
    }
}
