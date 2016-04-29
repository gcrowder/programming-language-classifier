import com.apple.cocoa.foundation.*;

class RandomMine extends SmallMine {

public void explode() {
    setVelocity(new NSSize(0f, Game.MAXVELY * (15f * Game.randInt(6)) / 100f));
}

}
