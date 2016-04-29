import com.apple.cocoa.foundation.*;
import java.util.Vector;

class Switch extends GamePiece {

public void initInGame(Game g) {
    initInGame(g, "switch", 1, 4);
    setPerFrameTime(10000000);
}

public void explode() {
    curPose = (curPose + 1) % numPoses;
    boolean open = (curPose == numPoses - 1);
    int pieceType, cnt;
    for (pieceType = StationaryEnemyPiece; pieceType <= MobileEnemyPiece; pieceType++) {
        Vector pieces = game.piecesOfType(pieceType);
        for (cnt = 0; cnt < pieces.size(); cnt++) {
            GamePiece piece = (GamePiece)(pieces.elementAt(cnt));
            if (piece instanceof SwitchedVertGate) {
                if (open) {
                    ((SwitchedVertGate)piece).openGate();
                } else {
                    ((SwitchedVertGate)piece).closeGate();
                }
            }
        }
    }
}

}
