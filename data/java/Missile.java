public class Missile extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "missile", 2);
    setPerFrameTime(100);
    setTimeToExpire(2000);
}

}

