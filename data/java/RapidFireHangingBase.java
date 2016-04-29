class RapidFireHangingBase extends HangingBase {

protected int rapidFire;

public void updatePiece() {
    boolean canFire = false;

    if (game.helicopter() == null) {
        rapidFire = 0;
    } else if (game.updateTime() > nextFireTime) {
        canFire = true;
    }

    super.updatePiece();

    if (canFire && (nextFireTime > game.updateTime())) {	// Did indeed fire!
        if (--rapidFire > 0) {
            nextFireTime = game.updateTime() + TIMETORECHARGERAPIDENEMYBULLET;
        } else {
            rapidFire = 4 + Game.randInt(6);
        }
    }
}

}
