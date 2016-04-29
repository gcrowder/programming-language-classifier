import com.apple.cocoa.foundation.*;

class SmartHangingBase extends RapidFireHangingBase {

protected int nextUpdateTime;	/* ms */

public void initInGame(Game g) {
    super.initInGame(g);
    setPerFrameTime(100000000);
}

public float detectDistance() {
    return SMARTHANGINGBASEDISTANCE;
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();
    if (helicopter != null) {
        NSRect helicopterRect = helicopter.rect();

        if (isInFrontAndWithin(detectDistance(), helicopter) && game.updateTime() > nextUpdateTime) {
            if (helicopterRect.maxY() >= pos.y()) {
                curImage = (curImage + 1) & 1;	// Images 0 & 1 point up
            } else {
                curImage = ((curImage + 1) & 3) | 2;	// 2 & 3 point down
            }
            nextUpdateTime = game.updateTime() + TIMETOADJUSTSMARTHANGINGBASE;
        }
    }
    super.updatePiece();
}

}
