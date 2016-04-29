class ToughSpider extends Spider {

public int rechargeTime() {
    if ((((game.updateTime() / 1000) % 10) == 0) || Game.randInt(12) == 0) {
        return TIMETOSLOWRECHARGETOUGHSPIDER;
    } else {
        return TIMETORECHARGETOUGHSPIDER;
    }
}

}
