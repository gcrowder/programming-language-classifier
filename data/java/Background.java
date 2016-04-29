/*
        Background.java
        Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        The background image for the game
        Reads and maintains landscape info; computes collisions with the background

 IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
 consideration of your agreement to the following terms, and your use, installation, 
 modification or redistribution of this Apple software constitutes acceptance of these 
 terms.  If you do not agree with these terms, please do not use, install, modify or 
 redistribute this Apple software.
 
 In consideration of your agreement to abide by the following terms, and subject to these 
 terms, Apple grants you a personal, non-exclusive license, under AppleÕs copyrights in 
 this original Apple software (the "Apple Software"), to use, reproduce, modify and 
 redistribute the Apple Software, with or without modifications, in source and/or binary 
 forms; provided that if you redistribute the Apple Software in its entirety and without 
 modifications, you must retain this notice and the following text and disclaimers in all 
 such redistributions of the Apple Software.  Neither the name, trademarks, service marks 
 or logos of Apple Computer, Inc. may be used to endorse or promote products derived from 
 the Apple Software without specific prior written permission from Apple. Except as expressly
 stated in this notice, no other rights or licenses, express or implied, are granted by Apple
 herein, including but not limited to any patent rights that may be infringed by your 
 derivative works or by other works in which the Apple Software may be incorporated.
 
 The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO WARRANTIES, 
 EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF NON-INFRINGEMENT, 
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS 
 USE AND OPERATION ALONE OR IN COMBINATION WITH YOUR PRODUCTS.
 
 IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR CONSEQUENTIAL 
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, 
 REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED AND 
 WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR 
 OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;
import java.io.*;
import java.util.StringTokenizer;

public class Background extends GamePiece {

public static final int LANDSCAPEHEIGHT = 175;
public static final int MAXLEVELNAMELENGTH = 80;
public static final int MAXLEVELS = 30;	/* Changing this will make highscores incompatible! */
public static final int MAXNUMLANDSCAPESPECS = 150;
public static final int LANDSCAPEWIDTHPERSPEC  = 25;	/* pixels */
public int maxLevelSpecs;		/* Maximum number of code points in any level */
public int numLandscapeSpecs;		/* Number of code points specified in cur level (actually one less) */
public int landscapeWidthInPixels;	/* Total width of landscape, and hence the background image; numLandscapeSpecs * LANDSCAPEWIDTHPERSPEC */
public NSRect gameRect;			/* Rect describing the current level game area */
    
protected short bottom[];	/* Describes the ceiling in the current level */
protected short top[];		/* Describes the ground in the current level */
protected int bottomSpec; 	/* Location of current level bottom spec in levelData */
protected int topSpec;		/* Location of current level top spec in levelData */
protected int gamePieces;	/* Location of current level pieces spec in levelData */
protected String levelDescription;
protected String fileName;
protected byte levelData[];
protected int currentLevel;
protected int numLevels;
protected NSColor groundColor;

protected float codeToHeight(int code) {
    return (float)(LANDSCAPEHEIGHT * ((code) - 'a') / (float)LANDSCAPEWIDTHPERSPEC);
}

protected int codeToWidth(int code) {
    return (int)(landscapeWidthInPixels * (code) / numLandscapeSpecs);
}

protected int widthToPrevCode(float w) {
    return (int)((w) / (landscapeWidthInPixels / numLandscapeSpecs));
}

protected int widthToNextCode(float w) {
    return (1 + (int)(((w) - 1) / (landscapeWidthInPixels / numLandscapeSpecs)));
}
        
public void initInGame(Game g) {

    String levelFile;

    if (g.isDemo()) {
        levelFile = NSBundle.mainBundle().pathForResource("DemoLevelData", "txt");
    } else {
        levelFile = NSUserDefaults.standardUserDefaults().stringForKey("LevelFile");
    }

    if (levelFile != null) {
	if (initializeLevelData(new NSData(new File(levelFile)))) {
	    fileName = NSPathUtilities.stringByDeletingPathExtension(NSPathUtilities.lastPathComponent(levelFile));
            if (!g.isDemo()) NSSystem.log("BlastApp: Using external level file " + levelFile + ".");
  	} else {
            NSAlertPanel.runAlert("Bad Level Data File", "The external level data file " + levelFile + " doesn't exist or is illegal. Will use the default level data instead.", "OK", null, null);
	}
    }		

    if (numLevels() == 0) {	// Indicating we haven't read the level data yet...
        levelFile = NSBundle.mainBundle().pathForResource("LevelData", "txt");
        if ((levelFile == null) || !initializeLevelData(new NSData(new File(levelFile)))) {
	    NSSystem.log("Bad level data, aborting.");
            NSApplication.sharedApplication().terminate(null);
	}
    }

    bottom = new short[maxLevelSpecs * LANDSCAPEWIDTHPERSPEC];
    top = new short[maxLevelSpecs * LANDSCAPEWIDTHPERSPEC];

    initInGame(g, new NSImage(new NSSize (maxLevelSpecs * LANDSCAPEWIDTHPERSPEC, LANDSCAPEHEIGHT)), 1, 1, false);
    setPerFrameTime(10000000);
}

public NSRect gameRect() {
    return gameRect;
}

public String levelDataName() {
    return fileName;
}

public int numLevels() {
    return numLevels;
}

// Returns the start of the next line; pins at end of the file or \0

private static int advanceToNextLine(byte ptr[], int cnt) {
    while (ptr[cnt] != 0 && ptr[cnt++] != '\n');
    return cnt;
}

// Loads the data and sets number of levels; if data is bad, returns false

public boolean initializeLevelData(NSData data) {
    levelData = data.bytes(0, data.length());
    boolean done = false;
    int cnt = 0, tmp;
    while (!done) {
        switch ((char)levelData[cnt]) {
            case '%':
                cnt = advanceToNextLine(levelData, cnt);
                break;
            case '+':
                numLevels++;
                cnt = advanceToNextLine(levelData, cnt);
                cnt = advanceToNextLine(levelData, cnt);
                tmp = advanceToNextLine(levelData, cnt);
                if (tmp - cnt - 2 > maxLevelSpecs) maxLevelSpecs = tmp - cnt - 2;	/* 1 for end of line, 1 for the extra spec */
                cnt = tmp;
                cnt = advanceToNextLine(levelData, cnt);
                cnt = advanceToNextLine(levelData, cnt);
                break;
            case '$':
		done = true;
		break;
            default:
                done = true;
                numLevels = 0;
                break;
        }
    }
    return numLevels > 0;
}    

public boolean getLevelData(int level) {
    int cnt = 0;
    int levelCnt = 0;

    while (levelCnt != level) {
        switch ((char)levelData[cnt]) {
	case '%':
            cnt = advanceToNextLine(levelData, cnt);
	    break;
	case '+':
	    levelCnt++;
	    int nameLoc = cnt + 1;
            cnt = advanceToNextLine(levelData, cnt);
            int colorLoc = cnt;
	    cnt = advanceToNextLine(levelData, cnt);
	    topSpec = cnt;
            cnt = advanceToNextLine(levelData, cnt);
            bottomSpec = cnt;
            cnt = advanceToNextLine(levelData, cnt);
            gamePieces = cnt;
            cnt = advanceToNextLine(levelData, cnt);
	    if (levelCnt == level) {
                cnt = advanceToNextLine(levelData, nameLoc);
                levelDescription = new String(levelData, nameLoc, cnt - nameLoc - 1);
                numLandscapeSpecs = bottomSpec - topSpec - 2;	/* One for EOL, one for the extra spec */
                landscapeWidthInPixels = numLandscapeSpecs * LANDSCAPEWIDTHPERSPEC;
                gameRect = new NSRect(0, 0, landscapeWidthInPixels, LANDSCAPEHEIGHT);
                try {
                    String colorSpec = new String(levelData, colorLoc, topSpec - colorLoc - 1);
		    if (colorSpec.equals("random")) {
			groundColor = NSColor.colorWithCalibratedHSB(Game.randInt(100)/100f, 1f, 1f, 1f);
		    } else {
                        StringTokenizer scanner = new StringTokenizer(colorSpec);
                        float red = Float.valueOf(scanner.nextToken()).floatValue();
                        float green = Float.valueOf(scanner.nextToken()).floatValue();
                        float blue = Float.valueOf(scanner.nextToken()).floatValue();
                        groundColor = NSColor.colorWithCalibratedRGB(red, green, blue, 1f);
		    }
                } catch (Exception e) {
                    NSSystem.log("Bad color specification for level " + levelCnt + "; using default.");
                    groundColor = NSColor.brownColor();
                }
	 	return true;
	    }
	    break;
	default:
	    return false;
        }
    }
    return false;
}    
	
public void setLevel(int level) {
    if (level > numLevels()) {
    	level = numLevels();
    }
    
    if (!getLevelData(level)) {
	NSAlertPanel.runAlert("Bad Level Data", "Sorry, level " + level + " is under construction.", "Bummer", null, null);
        while (--level != 0 && !getLevelData(level));
	if (level == 0) {
	    NSSystem.log("Bad level data file, aborting.");
            NSApplication.sharedApplication().terminate(null);
	}
    }

    currentLevel = level;

    createLandscape();
    images.lockFocus();
    drawLandscape();
    images.unlockFocus();
}

public String levelDescription() {
    return levelDescription;
}

protected static void washToBlack(NSColor color, NSSize size) {
    float hue = color.hueComponent();
    float curHeight;
    NSMutableRect rect = new NSMutableRect(0f, 0f, size.width(), 1f);
    for (curHeight = 0; curHeight < size.height(); curHeight++) {
        float brightness = 0.5f * (curHeight / size.height());
        NSColor.colorWithCalibratedHSB(hue, 1.0f, brightness, 1.0f).set();
        rect.setY(curHeight);
        NSBezierPath.fillRect(rect);
    }
}

protected static void washToWhite(NSColor color, NSSize size) {
    float hue = color.hueComponent();
    float curHeight;
    NSMutableRect rect = new NSMutableRect(0f, 0f, size.width(), 1f);
    for (curHeight = 0; curHeight < size.height(); curHeight++) {
        float saturation = 0.5f * (curHeight / size.height());
        NSColor.colorWithCalibratedHSB(hue, saturation, 1.0f, 1.0f).set();
        rect.setY(curHeight);
        NSBezierPath.fillRect(rect);
    }
}

public void drawLandscape() {
    NSColor color;
    int cnt;
    NSBezierPath path;
    NSMutablePoint point = new NSMutablePoint();

    color = groundColor;

    color.set();
    washToBlack(color, size());
        
    path = new NSBezierPath();
    point.setX(0f);
    point.setY(0f);
    path.moveToPoint(point);
    for (cnt = 0; cnt <= numLandscapeSpecs; cnt++) {
        point.setX((float)codeToWidth(cnt));
        point.setY((float)codeToHeight(levelData[topSpec + cnt]));
        path.lineToPoint(point);
    }
    for (cnt = numLandscapeSpecs; cnt >= 0; cnt--) {
        point.setX((float)codeToWidth(cnt));
        point.setY((float)codeToHeight(levelData[bottomSpec + cnt]));
	path.lineToPoint(point);
    }
    path.closePath();
    path.setClip();
    
    washToWhite(color, size());
}

public void draw(NSRect gameRect) {
    images.compositeToPointFromRect(NSPoint.ZeroPoint, gameRect, NSImage.CompositeCopy);
}

protected void createLandscape() {
    int spec, cnt;
    float vels[] = {Game.MAXVELY/2f, Game.MAXVELY/3f, Game.MAXVELY/4f, Game.MAXVELY/5f};	// vels is used to map 4 consecutive characters to velocities
    
    /* First create the elevation data to be used in collision detection. */
    
    for (spec = 0; spec < numLandscapeSpecs; spec++) {
        float bottomSlope = (float)(codeToHeight(levelData[bottomSpec + spec + 1]) - codeToHeight(levelData[bottomSpec + spec])) / (float)(codeToWidth(spec + 1) - codeToWidth(spec));
        float topSlope = (float)(codeToHeight(levelData[topSpec + spec + 1]) - codeToHeight(levelData[topSpec + spec])) / (float)(codeToWidth(spec + 1) - codeToWidth(spec));

	for (cnt = codeToWidth(spec); cnt < codeToWidth(spec + 1); cnt++) {
            bottom[cnt] = (short)(codeToHeight(levelData[bottomSpec + spec]) + (bottomSlope * (cnt - codeToWidth(spec))));
            top[cnt] = (short)(codeToHeight(levelData[topSpec + spec]) + (topSlope * (cnt - codeToWidth(spec))));
	}
    }

    spec = 0;
    while (spec < numLandscapeSpecs) {
	GamePiece piece = null;
	NSPoint location; 
        byte cur = levelData[gamePieces + spec];
        float bottomHeight = (float)Math.floor(codeToHeight(levelData[bottomSpec + spec]));
        float topHeight = (float)Math.floor(codeToHeight(levelData[topSpec + spec]));

	switch ((char)cur) {
            case '.':
                break;

            case 'a':
            case 'b':
            case 'F':
            case '2':
		switch((char)cur) {
                    case 'a': piece = new MissileBase(); break;
                    case 'b': piece = new SmartMissileBase();  break;
                    case 'F': piece = new RapidFireMissileBase();  break;
                    case '2': piece = new KillerMissileBase();  break;
		}
                piece.initInGame(game);
                piece.setLocation(new NSPoint((float)Math.floor(codeToWidth(spec) + codeToWidth(1) / 2f)  - (piece.size().width() / 2f), bottomHeight));
                game.addGamePiece(piece);
                break;

            case 'c':	
            case 'E': 	
            case 'J':	
            case '5':	
            case '6':
                switch((char)cur) {
                    case 'c': piece = new HangingBase(); break;
                    case 'E': piece = new RapidFireHangingBase(); break;	
                    case '5': piece = new DumHangingBase(); break;
                    case 'J': piece = new SmartHangingBase(); break;
                    case '6': piece = new SneakyHangingBase(); break;
                }
                piece.initInGame(game);
		piece.setLocation(new NSPoint((float)Math.floor(codeToWidth(spec) + codeToWidth(1) / 2f) - (piece.size().width() / 2), topHeight - (piece.size().height())));
                game.addGamePiece(piece);
	        break;

            case 'd':
            case 'e':
            case 'f':
            case 'g':
                putMine(new SmallMine(), spec, vels[cur - 'd']);
                break;

            case 'h':
                putMine(new SmallMine(), spec, Game.MAXVELY * (Game.randInt(60) / 100f));
                break;

            case 'j':
            case 'k':
            case 'l':
            case 'm':
                putMine(new Sentry(), spec, vels[cur - 'j']);
                break;

            case 'n':
                putMine(new Sentry(), spec, Game.MAXVELY * ((10f + Game.randInt(30)) / 100f));
                break;

            case 'o':
            case 'p':
            case 'q':
            case 'r':
                putMine(new FatSentry(), spec, vels[cur - 'o']);
                break;
            case 's':
                putMine(new FatSentry(), spec, Game.MAXVELY * ((10f + Game.randInt(40)) / 100f));
                break;

            case 't':
                putMine(new LargeMine(), spec, Game.MAXVELY/14f);
                break;

            case 'u':
                putMine(new Fog(), spec, (Game.oneIn(2) ? (Game.MAXVELY * (Game.randInt(10) / 200f)) : 0f));
                break;
	
            case 'w':
                putMine(new SmartMine(), spec, Game.MAXVELY * (0.25f + Game.randInt(30) / 100f));
		break;

            case 'z':
                putMine(new RealSmartMine(), spec, Game.MAXVELY * (0.4f + Game.randInt(30) / 100f));
                break;

            case 'v':	
                piece = new BackShooter();
                piece.initInGame(game);
		piece.setLocation(new NSPoint((float)Math.floor(codeToWidth(spec) + codeToWidth(1) / 2f) - (piece.size().width() / 2f), bottomHeight));
                game.addGamePiece(piece);
                break;

            case 'A':
            case 'B':
            case 'C':
                putMine(new AmeobaMine(), spec, vels[cur - 'A']);
                break;

            case 'D':
                putMine(new StopMine(), spec, Game.MAXVELY * ((10f + Game.randInt(40)) / 100f));
                break;

            case 'G':
                putMine(new RandomMine(), spec, Game.MAXVELY / 2f);
                break;

            case 'H':
                putMine(new Throton(), spec, Game.MAXVELY / 4f);
                break;

            case '8':
                putMine(new Throton(), spec, Game.MAXVELY / 3f);
                break;

            case 'I':
                putMine(new Throton(), spec, Game.MAXVELY * ((10f + Game.randInt(40)) / 100f));
                break;

            case 'L':
                putMine(new ProximityMine(), spec, 0f, 0f);
                break;

            case '@':
                putMine(new DonutMine(), spec, Game.MAXVELY, 0.5f);
                break;

            case 'x': 	
            case 'K':
            case 'M':	
            case 'O':	
            case 'P':	
                switch((char)cur) {
                    case 'x': piece = new DropShip(); break;
                    case 'K': piece = new RapidFireDropShip(); break;	
                    case 'M': piece = new AttackShip(); break;
                    case 'O': piece = new BigAttackShip(); break;
                    case 'P': piece = new SmartAttackShip(); break;
                }
                piece.initInGame(game);
		piece.setLocation(new NSPoint((float)Math.floor(codeToWidth(spec) + codeToWidth(1) / 2f), (float)Math.floor((bottomHeight + topHeight) / 2.0)));
                game.addGamePiece(piece);
                break;

            case 'y':
                piece = new ArrowBase();
                piece.initInGame(game);
		piece.setLocation(new NSPoint((float)Math.floor(codeToWidth(spec) + codeToWidth(1) / 2f) - (piece.size().width() / 2f), bottomHeight));
                game.addGamePiece(piece);
                break;

            case '+':	
                piece = new Switch();
                piece.initInGame(game);
		piece.setLocation(new NSPoint((float)Math.floor(codeToWidth(spec) + codeToWidth(1) / 2f), bottomHeight + 5f));
                game.addGamePiece(piece);
		break;

            case 'N':
                putMine(new Gunes(), spec, Game.MAXVELY * ((10f + Game.randInt(30)) / 100f));
                break;

            case '&':
                putMine(new FourMines(), spec, Game.MAXVELY / 4f);
                break;

            case 'Q':
            case 'R':
                putMine(new TimedVertGate(), spec, (cur == 'Q') ? 0f : Game.MAXVELY / 4f, 0.5f);
                break;

            case '*':
                putMine(new Hole(), spec, 0f, 0.5f);
                break;

            case 'S':
                putHorizMine(new GoodSheep(), spec, Game.MAXVELX / (Game.randInt(4) + 4f), 0f);
                break;

            case 'V':
                putHorizMine(new ToughGoodSheep(), spec, Game.MAXVELX / (Game.randInt(4) + 4f), 0f);
                break;

            case '4':
                putHorizMine(new BadSheep(), spec, Game.MAXVELX / (Game.randInt(4) + 4f), 0f);
                break;

            case 'U':
                putMine(new PassableVertGate(), spec, 0f, 0.5f);
                break;

            case '9':
                putMine(new SwitchedVertGate(), spec, 0f, 0.5f);
                break;

            case 'T':
                putMine(new TimedHorizGate(), spec, 0f, 0.5f);	/* ??? Should this be putHorizMine */
                break;

            case 'W':
                putMine(new WaveGenerator(), spec, Game.MAXVELY);
                break;

            case '$':
                putMine(new KillerWaveGenerator(), spec, Game.MAXVELY);
                break;

            case '3':
                putMine(new BombGenerator(), spec, 0f);
                break;

            case 'Y':
                putMine(new BoingBall(), spec, 0f, 0.8f);
                break;

            case 'X':
                putMine(new BubbleMine(), spec, 0f, (2 + Game.randInt(6))/10f);
                break;

            case 'Z':
                putMine(new BouncingBoingBall(), spec, Game.MAXVELY, 0f);
                break;

            case '7':
                putMine(new SneakyBoingBall(), spec, 0f, 0.95f);
                break;

            case '0':
                putHorizMine(new Spider(), spec, Game.MAXVELX / 3f, 0f);
                break;

            case '1':
                putHorizMine(new ToughSpider(), spec, Game.MAXVELX / 2.8f, 0f);
                break;

            default:
                NSSystem.log("Unknown game piece indicator " + cur);
                break;
            
	}
	spec++;
    }

}

// If locFromBottomPercentage >= 1.0, then random placement.

protected void putMine(Mine mine, int loc, float yVel, float locFromBottomPercentage) {
    float bottomHeight = 0f;
    float topHeight = 100000f;
    NSMutablePoint mineLocation = new NSMutablePoint(codeToWidth(loc), 0f);
    NSSize mineVelocity = new NSSize(0f, yVel);
    NSSize mineSize;
    int cnt;

    mine.initInGame(game);
    mineSize = mine.size();
    for (cnt = loc; cnt <= loc + widthToNextCode(mineSize.width()); cnt++) {
        bottomHeight = Game.maxFloat(bottomHeight, codeToHeight(levelData[bottomSpec + cnt]));
        topHeight = Game.minFloat(topHeight, codeToHeight(levelData[topSpec + cnt]));
    }
    bottomHeight += 2f;
    topHeight -= 2f;
    if (bottomHeight >= topHeight) {
	NSSystem.log("Can't place mine " + mine.toString() + " at " + loc + ".");
	return;
    }

    if (locFromBottomPercentage >= 1f) {
	mineLocation.setY(bottomHeight + Game.randInt((int)(topHeight - bottomHeight)));
    } else {
        mineLocation.setY(bottomHeight + (int)((topHeight - mineSize.height() - bottomHeight) * locFromBottomPercentage));
    }
    mine.setHighLow(topHeight, bottomHeight);
    mine.setVelocity(mineVelocity);
    mine.setLocation(mineLocation); 
    game.addGamePiece(mine);
}

protected void putHorizMine(HorizMine mine, int loc, float xVel, float locFromBottomPercentage) {
    float bottomHeight = codeToHeight(levelData[bottomSpec + loc]);
    float topHeight = 100000f;
    float leftLoc = 100000f;
    float rightLoc = -100000f;
    NSMutablePoint mineLocation = new NSMutablePoint(codeToWidth(loc), 0f);
    NSSize mineVelocity = new NSSize(xVel, 0f);
    NSSize mineSize;
    int cnt;

    mine.initInGame(game);
    mineSize = mine.size();
    for (cnt = loc; cnt <= loc + widthToNextCode(mineSize.width()); cnt++) {
        topHeight = Game.minFloat(topHeight, codeToHeight(levelData[topSpec + cnt]));
    }
    cnt = loc;
    while (cnt > 1 && (levelData[bottomSpec + loc] == levelData[bottomSpec + cnt - 1])) cnt--;
    leftLoc = codeToWidth(cnt);
    cnt = loc;
    while (cnt < numLandscapeSpecs - 1 && (levelData[bottomSpec + loc] == levelData[bottomSpec + cnt + 1])) cnt++;
    rightLoc = codeToWidth(cnt);

    if (locFromBottomPercentage >= 1f) {
	mineLocation.setY(bottomHeight + Game.randInt((int)(topHeight - mineSize.height() - bottomHeight)));
    } else {
        mineLocation.setY(bottomHeight + (int)((topHeight - bottomHeight) * locFromBottomPercentage));
    }
    mine.setLeftRight(leftLoc, rightLoc);
    mine.setVelocity(mineVelocity);
    mine.setLocation(mineLocation);
    game.addGamePiece(mine);
}

public void putMine(Mine mine, int loc, float yVel) {
    putMine(mine, loc, yVel, 1f);
}

public boolean touches(GamePiece obj) {
    return (obj != this && obj.touches(this));
}
 
public boolean touchesRect(NSRect rect) {
    int from = Game.minInt(Game.maxInt(0, (int)rect.x()), landscapeWidthInPixels-1);
    int to = Game.minInt(Game.maxInt(0, (int)rect.maxX()), landscapeWidthInPixels-1);

    while (from <= to) {
	if ((bottom[from] >= rect.y()) || (top[from] <= rect.maxY())) return true;
	from++;
    }

    return false;
}

// Returns the area that is clear between the specified start and end locations...

public NSRect clearRect(float startLoc, float endLoc) {
    int to = Game.maxInt(0, Game.minInt((int)endLoc, landscapeWidthInPixels - 1));
    int from = Game.maxInt(0, Game.minInt((int)startLoc, landscapeWidthInPixels - 1));
    int tmp;    
    float y1 = 0;
    float y2 = LANDSCAPEHEIGHT;

    for (tmp = from; tmp <= to; tmp++) {
        if (bottom[tmp] > y1) y1 = bottom[tmp];
	if (top[tmp] < y2) y2 = top[tmp];
    }
    return new NSRect(from, y1, to - from, y2 - y1);
}

}

