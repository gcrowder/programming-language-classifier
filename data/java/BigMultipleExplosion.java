import com.apple.cocoa.foundation.*;

class BigMultipleExplosion extends BigExplosion {

protected int generation;
protected int nextChangeTime;	/* ms */

public void initInGame(Game g) {
    super.initInGame(g);
    nextChangeTime = game.updateTime() + TIMETOEXPLODEBIGMULTIPLE;
}

public void setGeneration(int gen) {
    generation = gen;
}

public void updatePiece() {
    if (game.updateTime() > nextChangeTime && generation < MAXBIGMULTIPLEGENERATION) {
        NSSize newVel = new NSSize(vel.width() / 2f, vel.height() / 2f);
        int cnt;
        for (cnt = 0; cnt < 4; cnt += 1) {
            BigMultipleExplosion exp = new BigMultipleExplosion();	/* [self class] */
            exp.initInGame(game);
            NSPoint newLoc = new NSPoint(pos.x() + (((cnt & 1) != 0) ? 1 : -1) * (MINBIGMULTIPLEOFFSET + Game.randInt(MAXBIGMULTIPLEOFFSET - MINBIGMULTIPLEOFFSET)), pos.y() + (((cnt & 2) != 0) ? 1 : -1) * (MINBIGMULTIPLEOFFSET + Game.randInt(MAXBIGMULTIPLEOFFSET - MINBIGMULTIPLEOFFSET)));
            exp.setLocation(newLoc);
	    exp.setVelocity(newVel);
	    exp.setGeneration(generation + 1);
            game.addGamePiece(exp);
        }
        nextChangeTime = game.updateTime() + TIMETOEXPLODEBIGMULTIPLE;
        game.playExplosionSound();
    }
    super.updatePiece();
}

}
