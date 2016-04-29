import com.apple.cocoa.foundation.*;

class BoingBall extends Mine {

public void initInGame(Game g) {
    initInGame(g, "boing", 1);
    setPerFrameTime(100000000);
}

public boolean touches(GamePiece obj) {
    if (super.touches(obj)) {
	float third = (float)Math.floor(pos.width() / 3f);
        return (obj.touchesRect(new NSRect(pos.x() + third, pos.y() + 1f, third, pos.height() - 2f)) ||
                obj.touchesRect(new NSRect(pos.x() + 1f, pos.y() + third, pos.width() - 2f, third)));
    } else {
        return false;
    }
}

public boolean touchesRect(NSRect rect) {
    if (super.touchesRect(rect)) {
	float third = (float)Math.floor(pos.width() / 3f);
        return (intersectsRects(rect, new NSRect(pos.x() + third, pos.y() + 1f, third, pos.height() - 2f)) ||
	        intersectsRects(rect, new NSRect(pos.x() + 1f, pos.y() + third, pos.width() - 2f, third)));
    } else {
        return false;
    }
}

public void explode() {
    GamePiece exp = new BigMultipleExplosion();
    exp.initInGame(game);
    game.addScore(BOINGSCORE);
    super.explode(exp);
}

}
