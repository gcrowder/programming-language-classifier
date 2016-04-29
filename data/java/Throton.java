import com.apple.cocoa.foundation.*;

class Throton extends Mine {
protected boolean exploded;

public void initInGame(Game g) {
    initInGame(g, "throton", 3);
    setPerFrameTime(200);
}

public void explode() {
    if (!exploded) {
        game.addScore(MINESCORE);
        setPerFrameTime(1000);
        exploded = true;
	setVelocity(new NSSize(vel.width(), Game.MAXVELY / 6f));	// ??? Didn't use to call the method
    }
}

public void setLocation(NSPoint newLocation) {
    if (exploded && newLocation.y() < lowPoint) {
        newLocation = new NSPoint(newLocation.x(), Game.maxFloat(lowPoint, newLocation.y()));
        setVelocity(new NSSize(vel.width(), 0f));	// ??? Didn't use to call the method
        setPerFrameTime(4000);
    }
    super.setLocation(newLocation);
}

}
