import com.apple.cocoa.foundation.*;

class SneakyBoingBall extends BouncingBoingBall {

public void initInGame(Game g) {
    initInGame(g, "greenboing", 1);
    setPerFrameTime(100000000);
}

public void updatePiece() {
    GamePiece helicopter = game.helicopter();

    if (helicopter != null && acc.height() >= 0f && isInFrontAndWithin(3f, helicopter)) {
        setAcceleration(new NSSize(0, -25f));
        setVelocity(new NSSize(0, -Game.MAXVELX / 3f));
    }
    super.updatePiece();
}

public void explode() {
}

}
