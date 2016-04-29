import com.apple.cocoa.foundation.*;

class KillerMissile extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "smartmissile", 1);
    setPerFrameTime(TIMETOADJUSTKILLERMISSILE);
    setTimeToExpire(10000);
    setVelocity(new NSSize(0f, Game.MAXVELY / 3f));
}

public void frameChanged() {
    if (curImage == numImages - 1) {
        GamePiece helicopter = game.helicopter();
        if (helicopter != null) {
            setVelocity(new NSSize(Game.MAXVELX, Game.MAXVELY / 2f));
            flyTowards(helicopter, true);
        }
        setPerFrameTime(100000000);
    }
}

public void updatePiece() {
    if (game.helicopter() == null) {
        explode();
    } else {
        super.updatePiece();
    }
}

}
