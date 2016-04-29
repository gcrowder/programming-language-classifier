class BigExplosion extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "bigexplosion", 6);
    setPerFrameTime(TIMETOEXPIREBIGEXPLOSION / 6);
    setTimeToExpire(TIMETOEXPIREBIGEXPLOSION);
}

public void explode(GamePiece explosion) {
}

}
