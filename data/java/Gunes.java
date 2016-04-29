import com.apple.cocoa.foundation.*;

class Gunes extends Mine {

public void initInGame(Game g) {
    initInGame(g, "gunes", 8);
    setPerFrameTime(150);
}

public void explode() {
}

public boolean touches(GamePiece obj) {
    return false;
}

public boolean touchesRect(NSRect rect) {
    return false;
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && intersectsRects(helicopter.rect() , pos)) {
        NSSize hVel = helicopter.velocity();
        NSSize hAcc = helicopter.acceleration();
        if (Math.abs(hAcc.height()) > 1f || Math.abs(hAcc.width()) > 1f || Math.abs(hVel.height()) > 1f || Math.abs(hVel.width()) > Game.MAXVELX / 3f) {
            GamePiece exp = new BigExplosion();
	    exp.initInGame(game);
            explode(exp);
            return;
        }
    }
    super.updatePiece();
}

}
