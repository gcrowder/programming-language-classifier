import com.apple.cocoa.foundation.*;

class BackShooter extends GamePiece {

protected int nextFireTime; /* ms */

public int pieceType() {
    return StationaryEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "backshooter", 4);
    setPerFrameTime(300 + 5 * Game.randInt(20));
}

public boolean touches(GamePiece obj) {
    return false;
}

public void explode() {
    GamePiece exp = new BackShooterExplosion();
    exp.initInGame(game);
    game.addScore(BACKSHOOTERSCORE);
    explode(exp);
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();
    if (helicopter != null) {
        NSRect helicopterRect = helicopter.rect();

        if (isInBackAndWithin(BACKSHOOTERDISTANCE, helicopter) && ((helicopterRect.y() <= pos.maxY() && curImage == 2) || (helicopterRect.y() >= pos.maxY() && curImage == 0)) && game.updateTime() > nextFireTime) {
            GamePiece missile = new EnemyBullet();
	    missile.initInGame(game);
            missile.setVelocity(new NSSize(BULLETVEL, (curImage == 0) ? BULLETVEL/3f : 0f));
            missile.setLocation(new NSPoint(pos.maxX(), pos.y() + (curImage == 0 ? 7f : 4f)));
            game.addGamePiece(missile);
            game.playEnemyFireSound();
            nextFireTime = game.updateTime() + TIMETORECHARGEENEMYBULLET;
        }
    }
    super.updatePiece();
}

}
