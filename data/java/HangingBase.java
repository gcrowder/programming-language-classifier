import com.apple.cocoa.foundation.*;

class HangingBase extends DumHangingBase {

protected int nextFireTime;	/* ms */

public void explode() {
    GamePiece exp = new HangingBaseExplosion();
    exp.initInGame(game);
    game.addScore(HANGINGBASESCORE);
    explode(exp);
}

protected float detectDistance() {
    return HANGINGBASEDISTANCE;
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();
    if (helicopter != null) {
	NSRect helicopterRect = helicopter.rect();
        if (isInFrontAndWithin(detectDistance(), helicopter) && (curImage == 2 || (helicopterRect.maxY() >= pos.y() && curImage == 0)) && game.updateTime() > nextFireTime) {
            GamePiece missile = new EnemyBullet();
	    missile.initInGame(game);
            missile.setVelocity(new NSSize(-BULLETVEL, curImage < 2 ? 0f : -BULLETVEL/4f));
            missile.setLocation(new NSPoint(pos.x(), pos.y() + 2f));
            game.addGamePiece(missile);
            game.playEnemyFireSound();
            nextFireTime = game.updateTime() + TIMETORECHARGEENEMYBULLET;
        }
    }
    super.updatePiece();
}

}
