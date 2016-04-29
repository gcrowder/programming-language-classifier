import com.apple.cocoa.foundation.*;

class Hole extends Mine {

public void initInGame(Game g) {
    initInGame(g, "hole", 6, 1);
    setPerFrameTime(100);
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();
    NSSize heliAcc = null;

    if (helicopter != null && isWithin(HOLEDISTANCE, helicopter)) {
        NSRect hRect = helicopter.rect();
        float dist;
        NSPoint heliMidPoint;
        NSPoint holeMidPoint;
        heliMidPoint = new NSPoint(hRect.midX(), hRect.midY());
        holeMidPoint = new NSPoint(pos.midX(), pos.midY());
        dist = Game.distance(heliMidPoint, holeMidPoint);
        if (dist < HOLEDISTANCE) {
            float accMag = HOLEACCVALUE * (HOLEDISTANCE - dist) / HOLEDISTANCE;
            float xDelta = holeMidPoint.x() - heliMidPoint.x();
            float yDelta = holeMidPoint.y() - heliMidPoint.y();
            float tot = Math.abs(xDelta) + Math.abs(yDelta);
            heliAcc = new NSSize(accMag * xDelta / tot, accMag * yDelta / tot);
        }
    }
    if (helicopter != null) helicopter.setAcceleration(heliAcc == null ? NSSize.ZeroSize : heliAcc);	/* ??? Do this all the time? */
    super.updatePiece();
}

public void explode() {
}

public boolean touches(GamePiece obj) {
    if (super.touches(obj)) {
        return obj.touchesRect(new NSRect(pos.midX() - 2f, pos.midY() - 2f, 4f, 4f));
    } else {
        return false;
    }
}

public boolean touchesRect(NSRect rect) {
    if (super.touchesRect(rect)) {
        return intersectsRects(rect, (new NSRect(pos.midX() - 2f, pos.midY() - 2f, 4f, 4f)));
    } else {
        return false;
    }
}

}
