class SmartMineExplosion extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "smartmineexplosion", 4);
    setPerFrameTime(200);
    setTimeToExpire(800);
}

public void explode(GamePiece explosion) {
}

}
