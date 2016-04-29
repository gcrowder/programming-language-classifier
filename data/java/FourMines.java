import com.apple.cocoa.foundation.*;

class FourMines extends Mine {

public void initInGame(Game g) {
    initInGame(g, "fourmines", 10);
    setPerFrameTime(200);
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

    if ((curImage == 0) && helicopter != null && intersectsRects(helicopter.rect() , pos)) {
        GamePiece exp = new BigExplosion();
	exp.initInGame(game);
        explode(exp);
    } else {
        super.updatePiece();
    }
}

}
