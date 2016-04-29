import com.apple.cocoa.foundation.*;

class Spider extends HorizMine {

protected int nextFireTime;	/* ms */
protected int stunnedUntil;	/* ms */
int nHits;

public void initInGame(Game g) {
    initInGame(g, "spider", 3, 2);
    setPerFrameTime(300);
}

public int pieceType() {
    return StationaryEnemyPiece;
}

public void frameChanged() {
    if (curImage == 0) {
        GamePiece helicopter = game.helicopter();
        if (helicopter != null && isWithin(SPIDERDISTANCE, helicopter)) {
            NSRect hRect = helicopter.rect();
            setVelocity(new NSSize(((hRect.midX() < pos.x()) ? -Math.abs(vel.width()) : Math.abs(vel.width())), vel.height()));
        }
    }
}

protected static final int POINTINGLEFT = 1;
protected static final int POINTINGRIGHT = 0;

public void setVelocity(NSSize newVelocity) {
    super.setVelocity(newVelocity);
    if (vel.width() < 0) {
        curPose = POINTINGLEFT;
    } else {
        curPose = POINTINGRIGHT;
    }
}

public int rechargeTime() {
    return TIMETORECHARGESPIDER;
}

public void updatePiece() {
    if (game.updateTime() > stunnedUntil) {
        super.updatePiece();

        if (curImage != 0 && game.updateTime() > nextFireTime) {
            GamePiece helicopter = game.helicopter();
            if (helicopter!= null && isWithin(SPIDERDISTANCE, helicopter)) {
                fire(new EnemyBullet(), BULLETVEL, helicopter, true, null);
                nextFireTime = game.updateTime() + rechargeTime();
            }
        }
    }
}

public void explode() {
    if (game.updateTime() < stunnedUntil || ++nHits == REQUIREDSPIDERHITS) {
        nHits = 0;
        stunnedUntil = game.updateTime() + TIMETOUNSTUNSPIDER;
    }
}

}

	