import com.apple.cocoa.foundation.*;

class UntouchablePiece extends GamePiece {

public int pieceType() {
    return OtherPiece;
}

public boolean touches(GamePiece obj) {
    return false;
}

public boolean touchesRect(NSRect rect) {
    return false;
}

public void explode(GamePiece explosion) {
}

}

