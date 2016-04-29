public class HelicopterExplosion extends UntouchablePiece {

public void initInGame(Game g) {
    initInGame(g, "hexplosion", 10);
    setPerFrameTime(200);
    setTimeToExpire(2000);
}

}
