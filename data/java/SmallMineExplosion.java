class SmallMineExplosion extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "smallmineexplosion", 4);
    setPerFrameTime(200);
    setTimeToExpire(800);
}

public void explode(GamePiece explosion) {
}

}
