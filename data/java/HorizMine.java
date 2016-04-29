import com.apple.cocoa.foundation.*;

class HorizMine extends GamePiece {

protected float leftPoint, rightPoint;

public void setLeftRight(float left, float right) {
    leftPoint = left;
    rightPoint = right;
}

public void setLocation(NSPoint newLocation) {
    if (newLocation.x() + pos.width() > rightPoint || newLocation.x() < leftPoint) {
	setVelocity(new NSSize(-vel.width(), vel.height()));	/* ??? Didn't use to call the method */
        newLocation = new NSPoint(Game.minFloat(rightPoint - pos.width(), Game.maxFloat(leftPoint, newLocation.x())), newLocation.y());
    }
    super.setLocation(newLocation);
}

}
