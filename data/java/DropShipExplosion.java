class DropShipExplosion extends UntouchablePiece {

public void initInGame(Game g) {
    initInGame(g, "dropshipexplosion", 6);
    setPerFrameTime(200);
    setTimeToExpire(1000);
}

public void explode(GamePiece explosion) {
}

}
