import com.apple.cocoa.foundation.*;

class ToughGoodSheep extends HorizMine {

protected int nextUpdateTime;	/* ms */

public void initInGame(Game g) {
    initInGame(g, "sheep", 2, 4);
    setPerFrameTime(300);
}

public void updatePiece() {
    if (game.updateTime() > nextUpdateTime) {
        switch (Game.randInt(5)) {
            case 0: reverseVelocity(); break;
            case 1: curPose &= ~1; break;
            case 2: curPose |= 1; break;
            default: break;
        }
        nextUpdateTime = game.updateTime() + TIMETOADJUSTSHEEP;	
    }
    super.updatePiece();
    if (vel.width() < 0) {
        curPose &= ~2;
    } else {
        curPose |= 2;
    }
}

public void explode() {
}

public int pieceType() {
    return OtherPiece;
}

public boolean touches(GamePiece obj) {
    if (!super.touches(obj)) return false;	/* Easy case */
    return obj.touchesRect(new NSRect (pos.x(), pos.y(), pos.width(), pos.height() - 2f));
}

public boolean touchesRect(NSRect rect) {
    if (!super.touchesRect(rect)) return false;	/* Easy case */
    return intersectsRects(rect, new NSRect (pos.x(), pos.y(), pos.width(), pos.height() - 2f));
}

}
