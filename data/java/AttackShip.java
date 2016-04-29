import com.apple.cocoa.foundation.*;

class AttackShip extends GamePiece {

protected int nHits;

public void initInGame(Game g) {
    initInGame(g, "smartmine", 1);
    setPerFrameTime(100000000);
}

public void explode() {
    if (++nHits == REQUIREDATTACKSHIPHITS) {
        game.addScore(ATTACKSHIPSCORE);
        super.explode();
    }
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (vel.width() == 0 && helicopter != null && isWithin(ATTACKSHIPDISTANCE + Game.randInt(ATTACKSHIPDISTANCE/10), helicopter)) {
        NSRect hRect = helicopter.rect();
        NSSize newVel = new NSSize(-ATTACKSHIPVEL, Game.restrictValue(hRect.midY() - pos.midY(), Game.MAXVELY / 2f));
        setTimeToExpire(TIMETOEXPIREATTACKSHIP);
        setVelocity(newVel);	
    }
    super.updatePiece();
}

}
