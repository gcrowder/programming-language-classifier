class GoodSheep extends ToughGoodSheep {

public int pieceType() {
    return StationaryEnemyPiece;
}

public void explode() {
    game.addScore(GOODSHEEPSCORE);
    explode(null);
}

}
