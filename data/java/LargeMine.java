class LargeMine extends Mine {

public void initInGame(Game g) {
    initInGame(g, "largemine", 3);
    setPerFrameTime(150);
}

public void explode(GamePiece explosion) {
}

}
