class HangingBaseExplosion extends UntouchablePiece {

public void initInGame(Game g) {
    initInGame(g, "hbexplosion", 6);
    setPerFrameTime(150);
    setTimeToExpire(900);
}

}
