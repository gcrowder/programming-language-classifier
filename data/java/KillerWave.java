import com.apple.cocoa.foundation.*;

class KillerWave extends GamePiece {

protected int nHits;

public int pieceType() {
    return MobileEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "wave", 3);
    setVelocity(new NSSize(-Game.MAXVELX, 0f));
    setPerFrameTime(100);
    setTimeToExpire(5000);
}

public void explode() {
    int cnt;
    for (cnt = -1; cnt <= 1; cnt += 2) {
        GamePiece missile = new EnemyBullet();
 	missile.initInGame(game);
        missile.setVelocity(new NSSize(vel.width(), vel.height() + (cnt * Game.MAXVELY / 3f)));
        missile.setLocation(pos.origin());
        game.addGamePiece(missile);
    }
    game.addScore(ATTACKSHIPSCORE);
    explode(null);
}

}

