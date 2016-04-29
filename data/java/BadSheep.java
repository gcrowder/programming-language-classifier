class BadSheep extends GoodSheep {

public void explode() {
    game.addScore(BADSHEEPSCORE);
    explode(null);
}

}
