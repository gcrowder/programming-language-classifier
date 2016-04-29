import com.apple.cocoa.foundation.*;

class DropShip extends GamePiece {

protected int nextFireTime;	/* ms */

public void initInGame(Game g) {
    initInGame(g, "dropship", 1);
    setPerFrameTime(100000000);
}

/* 4x4 rectangle under the nose section is untouchable... */

public boolean touches(GamePiece obj) {
    if (super.touches(obj)) {
        return obj.touchesRect(new NSRect(pos.x(), pos.y() + 4f, pos.width(), pos.height() - 4f)) ||
	       obj.touchesRect(new NSRect(pos.x() + 4f, pos.y(), pos.width() - 4f, 4f));
    } else {
        return false;
    }
}

public boolean touchesRect(NSRect rect) {
    if (super.touchesRect(rect)) {
        return intersectsRects(rect, new NSRect(pos.x(), pos.y() + 4f, pos.width(), pos.height() - 4f)) ||
	       intersectsRects(rect, new NSRect(pos.x() + 4f, pos.y(), pos.width() - 4f, 4f));
    } else {
        return false;
    }
}

public void explode() {
    GamePiece exp = new DropShipExplosion();
    exp.initInGame(game);
    game.addScore(DROPSHIPSCORE);
    explode(exp);
}


public void updatePiece() {
   if (game.updateTime() > nextFireTime) {
        GamePiece helicopter = game.helicopter();
	NSSize newVelocity = null;
        if (helicopter != null && isInFrontAndWithin(DROPSHIPDISTANCE, helicopter)) {            
            NSRect helicopterRect = helicopter.rect();
            GamePiece missile = new EnemyBullet();
            missile.initInGame(game);
            NSPoint bulletLocation = new NSPoint(pos.x(), pos.y() + 7f);
            NSMutableSize bulletVelocity = new NSMutableSize(-BULLETVEL, 0f);
            if (helicopterRect.maxY() < pos.y()) {
                bulletVelocity.setHeight(-BULLETVEL/5f);
            } else if (helicopterRect.y() > pos.maxY()) {
                bulletVelocity.setHeight(BULLETVEL/5f);
            }
            missile.setVelocity(bulletVelocity);
            missile.setLocation(bulletLocation);
            game.addGamePiece(missile);
            game.playEnemyFireSound();
            newVelocity = new NSSize(0f, (helicopterRect.midY() - pos.midY()) / (float)Game.timeInSeconds(TIMETOADJUSTDROPSHIP));
            nextFireTime = game.updateTime() + TIMETOADJUSTDROPSHIP;
        }
        super.updatePiece();
        setVelocity(newVelocity == null ? NSSize.ZeroSize : newVelocity);
    } else {
        super.updatePiece();
    }
}

}
