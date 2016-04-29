public class Bullet extends GamePiece {

public int pieceType() {
    return FriendlyPiece;
}

public void initInGame(Game g) {
    initInGame(g, "bullet", 1);
    setPerFrameTime(2000);
    setTimeToExpire(2000);
}
	
}
