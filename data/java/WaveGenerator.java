import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;

class WaveGenerator extends Mine {

protected int nextFireTime;	/* ms */

public int pieceType() {
    return OtherPiece;
}

public void initInGame(Game g) {
    initInGame(g, (NSImage)null, 0, 0);
    setSize(new NSSize(0f, 6f));
    setPerFrameTime(10000000);
}

public GamePiece wave() {
    GamePiece wave = new Wave();
    wave.initInGame(game);
    return wave;
}

public void updatePiece() {
    NSRect gameSize;
    GamePiece helicopter = game.helicopter();

    gameSize = game.bounds();
    if (helicopter != null && game.updateTime() > nextFireTime && isInFrontAndBetween(gameSize.width(), gameSize.width()/2f, helicopter)) {
        GamePiece wave = wave();
        wave.setLocation(new NSPoint(pos.x(), pos.y()));
        game.addGamePiece(wave);
        nextFireTime = game.updateTime() + (TIMETORECHARGEWAVEGENERATOR * 3) / 4 + Game.randInt(TIMETORECHARGEWAVEGENERATOR / 2);
    }
    super.updatePiece();
}

}
