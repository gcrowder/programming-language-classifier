import com.apple.cocoa.foundation.*;

class BouncingBoingBall extends BoingBall {

public void initInGame(Game g) {
    super.initInGame(g);
    setAcceleration(new NSSize(0f, -25f));
}

}
