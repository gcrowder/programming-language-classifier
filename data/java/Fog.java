import com.apple.cocoa.foundation.*;

class Fog extends Mine {

public void initInGame(Game g) {
    initInGame(g, "fog", 7);
    setPerFrameTime(100000000);
    curImage = Game.randInt(numImages-1);
}


public void explode(GamePiece explosion) {
}

public boolean touches(GamePiece obj) {
    return false;
}

public boolean touchesRect(NSRect rect) {
    return false;
}

}
