import com.apple.cocoa.foundation.*;

class BubbleMine extends Mine {

public void initInGame(Game g) {
    initInGame(g, "bubblemine", 6);
    setPerFrameTime(90 + Game.randInt(10));
}

public boolean touches(GamePiece obj) {
    if (super.touches(obj)) {
        float third = (float)Math.floor(pos.width() / 3f);
        return (obj.touchesRect(new NSRect(pos.x() + third, pos.y() + 2f, third, pos.height() - 4f)) ||
                obj.touchesRect(new NSRect(pos.x() + 2f, pos.y() + third, pos.width() - 4f, third)));
    } else {
        return false;
    }
}

public boolean touchesRect(NSRect rect) {
    if (super.touchesRect(rect)) {
        float third = (float)Math.floor(pos.width() / 3f);
        return (intersectsRects(rect, new NSRect(pos.x() + third, pos.y() + 2f, third, pos.height() - 4f)) ||
                intersectsRects(rect, new NSRect(pos.x() + 2f, pos.y() + third, pos.width() - 4f, third)));
    } else {
        return false;
    }
}

public void explode() {
    int newPerFrameTime = perFrameTime + Game.randInt(32);
    if (newPerFrameTime > 250) {
        GamePiece exp = new BigMultipleExplosion();
	exp.initInGame(game);
        game.addScore(MINESCORE);
        super.explode(exp);
    } else {
        setPerFrameTime(newPerFrameTime, false);
    }
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && isWithin(BUBBLEMINEDISTANCE, helicopter)) {
        NSSize newVel;
        NSRect hRect = helicopter.rect();
        if (hRect.y() > pos.midY()) {
            newVel = new NSSize(0f, Game.MAXVELY / 10f);
        } else {
            newVel = new NSSize(0f, -Game.MAXVELY / 10f);
        }
        setVelocity(newVel);
    }
    if (perFrameTime > 100) {
        setPerFrameTime(perFrameTime - game.elapsedTime() / 20, false);
    }
    super.updatePiece();
}

}
