import com.apple.cocoa.foundation.*;

class Bomb extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "bomb", 6);
    setPerFrameTime(250);
    setTimeToExpire(8000);
}

public void frameChanged() {
    if (curImage == numImages - 1) {
        GamePiece helicopter = game.helicopter();
        if (helicopter != null) {
            NSRect hRect = helicopter.rect();
            NSSize backgroundSize = game.background().size();
            setVelocity(new NSSize(Game.MAXVELX, Game.MAXVELY * hRect.x() / (backgroundSize.width() * 2f)));
            flyTowards(helicopter, true);
        }
        setPerFrameTime(100000000);
    }
}

public void updatePiece() {
    if (game.helicopter() == null) {
	explode(null);
    } else {
	super.updatePiece();
    }
}

public void explode() {
    GamePiece exp = new SmallHarmlessExplosion();
    exp.initInGame(game);
    game.addScore(BOMBSCORE);
    super.explode(exp);
}
        
}
