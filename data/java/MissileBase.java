import com.apple.cocoa.foundation.*;

class MissileBase extends GamePiece {

protected int nextFireTime;	/* ms */

public int pieceType() {
    return StationaryEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "mbase", 3);
    setPerFrameTime(300 + 50 * Game.randInt(20));
}

public void explode() {
    GamePiece exp = new MissileBaseExplosion();
    exp.initInGame(game);
    game.addScore(MISSILEBASESCORE);
    explode(exp);
}

public void updatePiece() {
    if (isWithin(MISSILEDISTANCE, game.helicopter()) && curImage == 0 && game.updateTime() > nextFireTime) {
        GamePiece missile = new Missile();
        missile.initInGame(game);
        NSPoint bulletLocation = new NSPoint(pos.midX() - missile.size().width()/2f, pos.maxY() - 6f);
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

