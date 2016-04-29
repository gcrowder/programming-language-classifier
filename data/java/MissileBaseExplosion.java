class MissileBaseExplosion extends UntouchablePiece {

public void initInGame(Game g) {
    initInGame(g, "mbexplosion", 8);
    setPerFrameTime(100);
    setTimeToExpire(800);
}

}
