import com.apple.cocoa.foundation.*;

class Mine extends GamePiece {

protected float highPoint, lowPoint;

public void setHighLow(float hi, float lo) {
    highPoint = hi;
    lowPoint = lo;
}

public void setLocation(NSPoint newLocation) {
    if (newLocation.y() + pos.height() > highPoint || newLocation.y() < lowPoint) {
        setVelocity(new NSSize(vel.width(), -vel.height()));	/* ??? Didn't use to call the method */
        newLocation = new NSPoint(newLocation.x(), Game.minFloat(highPoint - pos.height(), Game.maxFloat(lowPoint, newLocation.y())));
    }
    super.setLocation(newLocation);
}

}
