import processing.core.PVector;

public class Piece {
    private int id;
    private float x;
    private float y;
    private float viewAngle;
    private int size;

    private int r;
    private int g;
    private int b;
    public Piece(String[] piece) {
        this.id = Integer.parseInt(piece[0]);
        setColor(piece[1]);
        this.x = Float.parseFloat(piece[2]);
        this.y = Float.parseFloat(piece[3]);
        if(this.viewAngle != -1) this.viewAngle = Float.parseFloat(piece[4]);
        System.out.println("Debug:Id: " + this.id + " X: " + this.x + " Y: " + this.y);
    }

    public void setColor(String color) {
        if(color.equals("white")) {
            this.r = 255;
            this.g = 255;
            this.b = 255;
            this.size = 50;
            this.viewAngle = 0;
        } else if (color.equals("black")) {
            this.r = 0;
            this.g = 0;
            this.b = 0;
            this.size = 50;
            this.viewAngle = 0;
        } else if(color.equals("red")) {
            this.r = 255;
            this.g = 0;
            this.b = 0;
            this.size = 20;
            this.viewAngle = -1;
        } else if(color.equals("green")) {
            this.r = 0;
            this.g = 255;
            this.b = 0;
            this.size = 20;
            this.viewAngle = -1;
        } else if(color.equals("blue")) {
            this.r = 0;
            this.g = 0;
            this.b = 255;
            this.size = 20;
            this.viewAngle = -1;
        }
    }

    public int getId() {
        return this.id;
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

    public float getViewAngle() {
        return this.viewAngle;
    }

    public float getSize() {
        return this.size;
    }
}
