class KillerWaveGenerator extends WaveGenerator {

public GamePiece wave() {
    GamePiece wave = new KillerWave();
    wave.initInGame(game);
    return wave;
}

}
