import processing.core.PVector;

public class Piece {
    private int id;
    private PVector pos;

    private int r;
    private int g;
    private int b;
    public Piece(String[] piece) {
        this.id = Integer.parseInt(piece[0]);
        setColor(piece[2]);
        this.pos.x = Float.parseFloat(piece[3]);
        this.pos.y = Float.parseFloat(piece[4]);
    }

    public void setColor(String color) {
        if(color.equals("red")) {
            this.r = 255;
            this.g = 0;
            this.b = 0;
        } else if(color.equals("green")) {
            this.r = 0;
            this.g = 255;
            this.b = 0;
        } else if(color.equals("blue")) {
            this.r = 0;
            this.g = 0;
            this.b = 255;
        }
    }
    public float getX() {
        return this.pos.x;
    }

    public float getY() {
        return this.pos.y;
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
        //Dependendo do size retorna algo
        return 10;
    }
}
