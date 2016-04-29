import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;

class BombGenerator extends Mine {

protected int nextFireTime;	/* ms */

public int pieceType() {
    return OtherPiece;
}

public void initInGame(Game g) {
    initInGame(g, (NSImage)null, 0, 0);
    setSize(new NSSize(0f, 6f));
    setPerFrameTime(100000000);
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && game.updateTime() > nextFireTime) {
        NSRect hRect = helicopter.rect();
        NSSize backgroundSize = game.background().size();
        if (hRect.midX() > hRect.width() / 2.0f + 10 && hRect.midX() < backgroundSize.width() - 200.0f) {
            GamePiece bullet = new Bomb();
            bullet.initInGame(game);
            bullet.setLocation(new NSPoint(hRect.midX() + 160f, 1 + Game.randInt((int)(highPoint - lowPoint - bullet.size().height()) - 1)));
            game.addGamePiece(bullet);
        }
        if (hRect.midX() > 200.0f) {
            GamePiece bullet = new Bomb();
            bullet.initInGame(game);
            bullet.setLocation(new NSPoint(hRect.midX() + 140f, 1 + Game.randInt((int)(highPoint - lowPoint - bullet.size().height()) - 1)));
            game.addGamePiece(bullet);
        }
        nextFireTime = game.updateTime() + TIMETORECHARGEBOMBGENERATOR;
    }
    super.updatePiece();
}

}
