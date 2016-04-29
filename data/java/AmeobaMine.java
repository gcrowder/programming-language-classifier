import com.apple.cocoa.foundation.*;

class AmeobaMine extends SmallMine {

protected int generation;

public void explode() {
    if (generation > MAXAMEOBAGENERATION) {
        super.explode();
    } else {
        AmeobaMine newMine = new AmeobaMine();
        newMine.initInGame(game);
        newMine.setHighLow(highPoint, lowPoint);
        newMine.setVelocity(new NSSize(-vel.width(), -vel.height()));
        newMine.setLocation(new NSPoint(pos.x(), pos.y()));
        newMine.setAcceleration(new NSSize(0f, 0f));
        setGeneration(generation + 1);
        newMine.setGeneration(generation + 1);
        game.addGamePiece(newMine);
    }
}

public void setGeneration(int gen) {
    generation = gen;
}

}
