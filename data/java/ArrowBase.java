import com.apple.cocoa.foundation.*;

class ArrowBase extends GamePiece {

protected int nextFireTime;	/* ms */

public int pieceType() {
    return StationaryEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "arrowbase", 1);
    setPerFrameTime(100000000);
}

public void explode() {
    GamePiece exp = new ArrowBaseExplosion();
    exp.initInGame(game);
    game.addScore(ARROWBASESCORE);
    explode(exp);
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();
    if (helicopter != null && isInFrontAndWithin(ARROWDISTANCE, helicopter) && game.updateTime() > nextFireTime) {
        GamePiece arrow = new Arrow();
        arrow.initInGame(game);
        arrow.setVelocity(new NSSize(-ARROWVEL, 0f));
        arrow.setLocation(new NSPoint(pos.x() - 7f, pos.y() + 11f));
        game.addGamePiece(arrow);
        game.playEnemyFireSound();
        nextFireTime = game.updateTime() + TIMETORECHARGEARROW;
    }
    super.updatePiece();
}

}
