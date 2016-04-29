class SmallHarmlessExplosion extends UntouchablePiece {

public void initInGame(Game g) {
    initInGame(g, "smallexplosion", 8);
    setPerFrameTime(100);
    setTimeToExpire(800);
}

}
