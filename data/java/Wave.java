import com.apple.cocoa.foundation.*;

class Wave extends GamePiece {

protected int nHits;

public int pieceType() {
    return MobileEnemyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "wave", 3);
    setVelocity(new NSSize(-Game.MAXVELX, 0f));
    setPerFrameTime(100);
    setTimeToExpire(6000);
}

public void explode() {
    if (++nHits == REQUIREDWAVEHITS) {
        game.addScore(ATTACKSHIPSCORE);
        super.explode();
    }
}

// This makes all of the Wave instances disappear when the helicopter goes away. Cheezy.
public void updatePiece() {
    if (game.helicopter() == null) {
        super.explode();
    } else {
        super.updatePiece();
    }
}

}
