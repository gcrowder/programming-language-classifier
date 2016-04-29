import com.apple.cocoa.foundation.*;

class SmartMissileBase extends GamePiece {

protected int nextFireTime;	/* ms */

public int pieceType() {
    return StationaryEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "smartmbase", 3);
    setPerFrameTime(600);
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
    if (isWithin(SMARTMISSILEDISTANCE, game.helicopter()) && curImage == 0 && game.updateTime() > nextFireTime) {
        GamePiece missile = new SmartMissile();
        missile.initInGame(game);
        NSPoint bulletLocation = new NSPoint(pos.midX() - 3f, pos.maxY());
        NSSize bulletVelocity = new NSSize(vel.width(), vel.height() + BULLETVEL);
        missile.setLocation(bulletLocation);
        missile.setVelocity(bulletVelocity);
        game.addGamePiece(missile);
        game.playEnemyFireSound();
        nextFireTime = game.updateTime() + TIMETORECHARGEMISSILE;
    }
    super.updatePiece();
}

}
