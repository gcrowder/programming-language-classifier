import com.apple.cocoa.foundation.*;

class SmartMissile extends GamePiece {

protected int nextAdjustTime;	/* ms */

public void initInGame(Game g) {
    initInGame(g, "smartmissile", 1);
    setPerFrameTime(2500);
    setTimeToExpire(2500);
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && game.updateTime() > nextAdjustTime) {
        NSRect hRect = helicopter.rect();
        setAcceleration(new NSSize(2f * (hRect.midX() - pos.x() - vel.width()),  2f * (hRect.midY() - pos.y() - vel.height())));
        nextAdjustTime = game.updateTime() + TIMETOADJUSTSMARTMISSILE;
    }
    super.updatePiece();
}

}
