import com.apple.cocoa.foundation.*;

class HorizGate extends Gate {

public void initInGame(Game g) {
    initInGame(g, "hblock", 1, 2);
    setPerFrameTime(10000000);
}

public boolean touches(GamePiece obj) {
    if (super.touches(obj)) {
        if (isClosed()) return true;	// If closed, the whole gate is bad...
        if (obj.touchesRect(new NSRect(pos.x(), pos.y(), 5f, 5f)) || obj.touchesRect(new NSRect(pos.maxX() - 5f, pos.y(), 5f, 5f))) return true;
    }
    return false;
}

public boolean touchesRect(NSRect rect) {
    if (super.touchesRect(rect)) {
        if (isClosed()) return true;	// If closed, the whole gate is bad...
        if (intersectsRects(rect, new NSRect(pos.x(), pos.y(), 5f, 5f)) || intersectsRects(rect, new NSRect(pos.maxX() - 5f, pos.y(), 5f, 5f))) return true;
    }
    return false;
}

}
