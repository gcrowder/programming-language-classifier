class DumHangingBase extends GamePiece {

public int pieceType() {
    return StationaryEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "hbase", 4);
    setPerFrameTime(500 + 20 * Game.randInt(10));
}

public void explode() {
}

}
