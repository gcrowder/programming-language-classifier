class SmallMine extends Mine {

public void initInGame(Game g) {
    initInGame(g, "smallmine", 1);
    setPerFrameTime(100000000);
}

public void explode() {
    GamePiece exp = new SmallMineExplosion();
    exp.initInGame(game);
    game.addScore(MINESCORE);
    explode(exp);
}

}
