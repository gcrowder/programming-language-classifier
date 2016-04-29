import com.apple.cocoa.foundation.*;

class Arrow extends GamePiece {

protected int nextUpdateTime;	/* ms */

public void initInGame(Game g) {
    initInGame(g, "arrow", 7);
    setPerFrameTime(100);
    setTimeToExpire(2500);
    nextUpdateTime = game.updateTime() + TIMETOADJUSTARROW;
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && game.updateTime() > nextUpdateTime) {
        NSRect hRect = helicopter.rect();
        NSSize newVelocity = new NSSize(vel.width(), Game.restrictValue((hRect.midY() - pos.midY()) / (float)Game.timeInSeconds(TIMETOADJUSTARROW), Game.MAXVELY / 2f));
        setVelocity(newVelocity);
        nextUpdateTime = game.updateTime() + TIMETOADJUSTARROW;
    }
    super.updatePiece();
}

}
