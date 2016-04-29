import com.apple.cocoa.foundation.*;

class SmartMine extends Mine {

protected int nHits;

public void initInGame(Game g) {
    initInGame(g, "smartmine", 1);
    setPerFrameTime(100000000);
}

public int requiredHits() {
    return REQUIREDSMARTMINEHITS;
}

public float detectDistance() {
    return SMARTMINEDISTANCE;
}

public void explode() {
    if (++nHits == requiredHits()) {
        GamePiece exp = new SmartMineExplosion();
        exp.initInGame(game);
        game.addScore(MINESCORE);
        explode(exp);
    }
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && isWithin(detectDistance(), helicopter)) {
        NSRect hRect = helicopter.rect();
        if ((hRect.y() > pos.maxY() && vel.height() < 0f) || (hRect.maxY() < pos.y() && vel.height() > 0f)) {
            setVelocity(new NSSize(0f, -vel.height()));	
        }
    }
    super.updatePiece();
}

}
