import com.apple.cocoa.foundation.*;

class DonutMine extends Mine {

public void initInGame(Game g) {
    initInGame(g, "donut", 1);
    setPerFrameTime(100000000);
}

public void explode() {
}

public void setLocation(NSPoint newLocation) {
    NSSize tmpVel = vel;
    super.setLocation(newLocation);
    if (tmpVel != vel) {
        if (Game.randInt(2) == 0 && game.helicopter() != null && isWithin(DONUTDISTANCE, game.helicopter())) {
	    setVelocity(new NSSize(tmpVel.width(), ((vel.height() < 0) ? -1 : 1) * Game.MAXVELY));
        } else {
            setVelocity(new NSSize(tmpVel.width(), ((vel.height() < 0) ? -1 : 1) * Game.MAXVELY / 5f));
        }
    }
}

}
