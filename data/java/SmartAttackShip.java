import com.apple.cocoa.foundation.*;

class SmartAttackShip extends BigAttackShip {

protected int nextAdjustTime;	/* ms */

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if ((vel.width() != 0) && helicopter != null && game.updateTime() > nextAdjustTime) {
        NSRect hRect = helicopter.rect();
        NSSize newVel = new NSSize(vel.width(), Game.restrictValue(hRect.midY() - pos.midY(), Game.MAXVELY / 2f));
        setVelocity(newVel);
        nextAdjustTime = game.updateTime() + TIMETOADJUSTATTACKSHIP;	
    }
    super.updatePiece();
}

}
