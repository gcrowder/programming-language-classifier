class ProximityMine extends Mine {

protected int explodeTime;	/* ms */

public void initInGame(Game g) {
    initInGame(g, "proximitymine", 1);
    setPerFrameTime(100000000);
}

public void updatePiece() {
    if (explodeTime == 0 && game.helicopter() != null && isWithin(PROXIMITYMINEDISTANCE, game.helicopter())) {
        explodeTime = game.updateTime() + TIMETODETONATEPROXIMITYMINE;
    } else if (explodeTime > 0) {
        if (game.helicopter() == null) {
            explodeTime = 0;	// Abort explosion...
        } else if (game.updateTime() > explodeTime) {
            GamePiece exp = new BigExplosion();
	    exp.initInGame(game);
	    super.explode(exp);
            return;
        }
    }
    super.updatePiece();
}

public void explode() {
}

}
