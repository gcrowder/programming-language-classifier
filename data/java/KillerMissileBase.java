import com.apple.cocoa.foundation.*;

class KillerMissileBase extends GamePiece {

protected int nextFireTime;	/* ms */

public int pieceType() {
    return StationaryEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "smartmbase", 3);
    setPerFrameTime(200);
}

public boolean touches(GamePiece obj) {
    return false;
}

public void explode() {
    GamePiece exp = new SmartMissileBaseExplosion();
    exp.initInGame(game);
    game.addScore(MISSILEBASESCORE);
    explode(exp);
}

public void updatePiece() {
    if (isWithin(KILLERMISSILEDISTANCE, game.helicopter()) && game.updateTime() > nextFireTime) {
        GamePiece missile = new KillerMissile();
	missile.initInGame(game);
        missile.setLocation(new NSPoint(pos.midX() - 3f, pos.maxY()));
        game.addGamePiece(missile);
        game.playEnemyFireSound();
        nextFireTime = game.updateTime() + TIMETORECHARGEKILLERMISSILE;
    }
    super.updatePiece();
}

}
