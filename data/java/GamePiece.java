/*
        GamePiece.java
        Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        The mostly abstract superclass of all objects in the game
*/
/*
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
import java.util.Vector;

public class GamePiece {

/* Default time (in milliseconds) per frame of animation */
protected static final int DEFAULTPERFRAMETIME = 200;	/* ms */

/* Velocities in pixels/sec */
protected static final int BULLETVEL = 65;
protected static final int ARROWVEL = 90;
protected static final int ATTACKSHIPVEL = 90;

/* Time to recharge the helicopter's gun */
protected static final int TIMETORECHARGE = 500;

/* Time to recharge the missile bases and adjust various smart missiles */
protected static final int TIMETORECHARGEMISSILE = 2000;
protected static final int TIMETORECHARGESMARTMISSILE = 4000;
protected static final int TIMETORECHARGEENEMYBULLET = 3000;
protected static final int TIMETORECHARGERAPIDENEMYBULLET = 200;
protected static final int TIMETORECHARGERAPIDMISSILE = 300;
protected static final int TIMETORECHARGEARROW = 4000;
protected static final int TIMETOADJUSTSMARTMISSILE = 500;
protected static final int TIMETOADJUSTDROPSHIP = 1000;
protected static final int TIMETOADJUSTARROW = 300;
protected static final int TIMETOADJUSTSMARTHANGINGBASE = 300;
protected static final int TIMETODETONATEPROXIMITYMINE = 2000;
protected static final int TIMETOEXPIREATTACKSHIP = 2000;
protected static final int TIMETOADJUSTATTACKSHIP = 700;
protected static final int TIMETOADJUSTSHEEP = 2000;
protected static final int TIMETOEXPIREBIGEXPLOSION = 600;
protected static final int TIMETOEXPLODEBIGMULTIPLE = (TIMETOEXPIREBIGEXPLOSION * 2 / 3);
protected static final int TIMETORECHARGEWAVEGENERATOR = 800;
protected static final int TIMETORECHARGEBOMBGENERATOR = 2000;
protected static final int TIMETORECHARGESPIDER = 8000;
protected static final int TIMETORECHARGETOUGHSPIDER = 500;
protected static final int TIMETOSLOWRECHARGETOUGHSPIDER = 2500;
protected static final int TIMETOUNSTUNSPIDER = 12000;
protected static final int TIMETOADJUSTKILLERMISSILE = 1000;
protected static final int TIMETOADJUSTBOMB = 1000;
protected static final int TIMETORECHARGEKILLERMISSILE = 2000;

protected static final int TIMETOCHANGEGATE = 2000;
protected static final int TIMETOOPENGATE = 100;
protected static final int TIMETOCLOSEGATE = 2000;
protected static final int TIMETOCLOSEAUTOGATE = 6000;
protected static final int TIMETOCHANGESWITCHEDGATE = 1000;
protected static final int TIMETOCLOSESWITCHEDGATE = 10000;

// Distance within which the various enemy bases will fire
// (Note that helicopter bullet speed is 65 and it lives for 2 seconds)
protected static final int MISSILEDISTANCE = 25;
protected static final int HANGINGBASEDISTANCE = 125;
protected static final int SMARTHANGINGBASEDISTANCE = 140;
protected static final int SNEAKYHANGINGBASEDISTANCE = 40;
protected static final int SMARTMISSILEDISTANCE = 75;
protected static final int KILLERMISSILEDISTANCE = 200;
protected static final int SMARTMINEDISTANCE = 40;
protected static final int ARROWDISTANCE = 250;
protected static final int DROPSHIPDISTANCE = 160;
protected static final int BACKSHOOTERDISTANCE = 250;
protected static final int PROXIMITYMINEDISTANCE = 20;
protected static final int ATTACKSHIPDISTANCE = 125;
// If this is increased might have to fix level 20
protected static final int MAXBIGMULTIPLEOFFSET = 35;
protected static final int MINBIGMULTIPLEOFFSET = 10;
protected static final int SPIDERDISTANCE = 150;
protected static final int BUBBLEMINEDISTANCE = 60;
protected static final int HOLEDISTANCE = 200;
protected static final int DONUTDISTANCE = 30;

protected static final int MISSILEBASESCORE = 1;
protected static final int HANGINGBASESCORE = 1;
protected static final int MINESCORE = 1;
protected static final int BACKSHOOTERSCORE = 1;
protected static final int DROPSHIPSCORE = 1;
protected static final int ARROWBASESCORE = 1;
protected static final int ATTACKSHIPSCORE = 1;
protected static final int GOODSHEEPSCORE = 2;
protected static final int BADSHEEPSCORE = 1;
protected static final int BOINGSCORE = 1;
protected static final int SPIDERSCORE = 1;
protected static final int BOMBSCORE = 1;

protected static final int REQUIREDSMARTMINEHITS = 3;
protected static final int REQUIREDATTACKSHIPHITS = 3;
protected static final int REQUIREDWAVEHITS = 2;
protected static final int REQUIREDSPIDERHITS = 2;

protected static final int MAXAMEOBAGENERATION = 3;
protected static final int MAXBIGMULTIPLEGENERATION = 1;

protected static final int HELICOPTERACCVALUE = 120;
protected static final int HOLEACCVALUE = 80;

// Types of game pieces
protected static final int FriendlyPiece = 0;
protected static final int StationaryEnemyPiece = 1;
protected static final int MobileEnemyPiece = 2;
protected static final int OtherPiece = 3;
protected static final int LastGamePiece = 4;

public NSSize vel;
public NSSize acc;
public NSRect pos;
protected NSImage images;
protected int numImages;
protected int curImage;
protected int numPoses;
protected int curPose;
protected int aliveUntil;		/* ms */
protected int perFrameTime;		/* ms */
protected int nextFrameTime;		/* ms */
protected Game game;

/* Initializes a game piece from the specified arguments. 

Note that a single image is used for all the possible images of a single piece. The frames are vertically stacked; the various poses (a few pieces have poses, depending on what they're doing) are horizontally stacks. nf and np define the number of frames & poses.
*/
public void initInGame(Game g, NSImage frames, int nf, int np, boolean cacheFlag) {
    game = g;
    numImages = nf;
    numPoses = np;
    curImage = 0;
    curPose = 0;
    pos = NSRect.ZeroRect;
    vel = NSSize.ZeroSize;
    acc = NSSize.ZeroSize;

    if (frames != null) {
	images = frames;
	NSSize imageSize = images.size();
        // Frames are vertically stacked, poses are horizontally stacked...
        setSize(new NSSize(imageSize.width() / numPoses, imageSize.height() / numImages));
	if (cacheFlag) cacheImage(images);
    }

    setPerFrameTime(DEFAULTPERFRAMETIME);
    aliveUntil = 0;		// This value indicates the object will not die
}

public void initInGame(Game g, NSImage frames, int nf, int np) {
    initInGame(g, frames, nf, np, true);
}

public void initInGame(Game g, String imageName, int nf, int np) {
    initInGame(g, (NSImage)NSImage.imageNamed(imageName), nf, np);
}

public void initInGame(Game g, String imageName, int nf) {
    initInGame(g, imageName, nf, 1);
}

public void initInGame(Game g) {
    initInGame(g, null, 1, 1, true);
}
               
public void setSize(NSSize newSize) {
    pos = new NSRect(pos.origin(), newSize);
}

public void setLocation(NSPoint newLocation) {
    pos = new NSRect(newLocation, pos.size());
}

public void setPerFrameTime(int time) {
    setPerFrameTime(time, true);
}

public void setPerFrameTime(int time, boolean takeEffectNow) {
    perFrameTime = time;
    if (takeEffectNow) nextFrameTime = game.updateTime() + perFrameTime;
}

public NSPoint location() {
    return pos.origin();
}

public NSSize size() {
    return pos.size();
}

public NSRect rect() {
    return pos;
}

public void reverseVelocity() {
    NSSize newVel = new NSSize(-vel.width(), -vel.height());
    setVelocity(newVel);
}

public void setVelocity(NSSize newVel) {
    vel = new NSSize(newVel);
}

public NSSize velocity() {
    return vel;
}

public void setAcceleration(NSSize newAcc) {
    acc = new NSSize(newAcc);
}

public NSSize acceleration() {
    return acc;
}

public void setTimeToExpire(int time) {
    aliveUntil = game.updateTime() + time;
}

public void frameChanged() {
}

public boolean touches(GamePiece obj) {
    return (obj != null && obj != this && obj.touchesRect(pos));
}

public static boolean intersectsRects(NSRect a, NSRect b) {
    if (a.width() == 0 || a.height() == 0) return false;
    if (b.width() == 0 || b.height() == 0) return false;
    if (a.x() >= b.maxX()) return false;
    if (b.x() >= a.maxX()) return false;
    if (a.y() >= b.maxY()) return false;
    if (b.y() >= a.maxY()) return false;
    return true;
}

public boolean touchesRect(NSRect rect) {
    return intersectsRects(pos, rect);
}

public boolean isWithin(float dist, GamePiece obj) {	// Horizontal proximity
    if (obj == null) return false;
    NSRect objRect = obj.rect();
    return (pos.x() - dist < objRect.maxX()) && (pos.maxX() + dist > objRect.x());
}

public boolean isInFrontAndWithin(float dist, GamePiece obj) {
    if (obj == null) return false;
    NSRect objRect = obj.rect();
    return (pos.x() - dist < objRect.maxX()) && (pos.maxX() > objRect.x());
}

public boolean isInBackAndWithin(float dist, GamePiece obj) {
    if (obj == null) return false;
    NSRect objRect = obj.rect();
    return (objRect.x() - pos.maxX() < dist) && (objRect.maxX() > pos.x());
}

public boolean isInFrontAndBetween(float farDist, float nearDist, GamePiece obj) {
    if (obj == null) return false;
    NSRect objRect = obj.rect();
    return (pos.x() - farDist < objRect.maxX()) && (pos.maxX() > objRect.x() + nearDist);
}

public void updatePiece() {
    int updateTime = game.updateTime();
    int t = game.elapsedTime();

    if (t > 0) {

	if (aliveUntil > 0 && updateTime > aliveUntil) {
	    game.removeGamePiece(this);
	    return;
	}

	// Check to see if its time to jump to the next frame.

	if (images != null && (updateTime > nextFrameTime)) {
            nextFrameTime = Game.maxInt(nextFrameTime + perFrameTime, updateTime + perFrameTime / 2);
	    curImage = (curImage + 1) % numImages;
	    frameChanged();
	}

	// Determine new velocity & location

	NSSize oldVel = vel;
        NSSize curAcc = acceleration();	// Due to goofiness in helicopter's implementation...
        float timeInSeconds = (float)Game.timeInSeconds(t);
        if (curAcc.width() != 0f || curAcc.height() != 0f) {	// Avoid creating a Size
            setVelocity(new NSSize(vel.width() + curAcc.width() * timeInSeconds, vel.height() + curAcc.height() * timeInSeconds));
        }
        if (oldVel.width() != 0f || oldVel.height() != 0f || vel.width() != 0f || vel.height() != 0f) {	// Avoid creating a Point
            setLocation(new NSPoint(pos.x() + 0.5f * (oldVel.width() + vel.width()) * timeInSeconds, pos.y() + 0.5f * (oldVel.height() + vel.height()) * timeInSeconds));
        }
    }
}

public boolean isVisibleIn(NSRect rect) {
    return intersectsRects(rect, pos);
}

public void draw(NSRect gameRect) {
    if (isVisibleIn(gameRect) && images != null) {
        NSRect fromRect = new NSRect(pos.width() * curPose, pos.height() * curImage, pos.width(), pos.height());
	NSPoint toPoint = new NSPoint((float)Math.floor(pos.x() - gameRect.x()), (float)Math.floor(pos.y() - gameRect.y()));
	images.compositeToPointFromRect(toPoint, fromRect, NSImage.CompositeSourceOver);
    }
}

public void explode() {
    explode(null);
}

/* Redeclared here to allow overriding...
*/
public void playExplosionSound() {
    game.playExplosionSound();
}

public void explode(GamePiece explosion) {
    if (explosion != null) {
	NSSize explosionSize = explosion.size();
	NSMutablePoint loc = new NSMutablePoint(location());
	playExplosionSound();
	loc.setX(pos.x() - (explosionSize.width() - pos.width()) / 2);
	loc.setY(pos.y() - (explosionSize.height() - pos.height()) / 2);
	explosion.setVelocity(vel);
	explosion.setLocation(loc);
	game.addGamePiece(explosion);
    }
    game.removeGamePiece(this);
}

public int pieceType() {
    return MobileEnemyPiece;
}

public void flyTowards(GamePiece obj, boolean smart) {
    NSRect objRect = obj.rect();
    if (smart) {
        NSSize objVel = obj.velocity();
        objRect = new NSRect(objRect.x() + (objVel.width() * 0.6f), objRect.y() + (objVel.height() * 0.4f), objRect.width(), objRect.height());
    }
    NSMutableSize velVector = new NSMutableSize(objRect.midX() - pos.x(), objRect.midY() - pos.y());
    float mag = (float)Math.sqrt(velVector.width() * velVector.width() + velVector.height() * velVector.height());
    float speed = (float)Math.sqrt(vel.width() * vel.width() + vel.height() * vel.height());
    velVector.setWidth(velVector.width() * speed / mag);
    velVector.setHeight(velVector.height() * speed / mag);
    setVelocity(velVector);
}

// This method allows pieces to fire (shoot) other pieces
// bullet should already be created but not initted
// fireFrom specifies the offset to fire from; firefrom == null mean fire from middle

public void fire(GamePiece bullet, float speed, GamePiece obj, boolean smart, NSPoint fireFrom) {	
    NSSize newVel = new NSSize(speed, 0f);
    NSPoint bulletLoc;
    bullet.initInGame(game);

    if (fireFrom != null) {	// If firing location specified, fire from there
	bulletLoc = new NSPoint(pos.x() + fireFrom.x(), pos.y() + fireFrom.y());
    } else {		// else fire from the middle
	NSSize bulletSize = bullet.size();
	bulletLoc = new NSPoint(pos.midX() - bulletSize.width() / 2.0f, pos.midY() - bulletSize.height() / 2.0f);
    }
    bullet.setLocation(bulletLoc);
    bullet.setVelocity(newVel);
    game.addGamePiece(bullet);
    game.playEnemyFireSound();
    bullet.flyTowards(obj, smart);
}

// Hold images which have been explicitly cached...

protected static Vector cachedImages = new Vector();

public static void cacheImage(NSImage image) {
    if (!cachedImages.contains(image)) {
        image.lockFocus();
        image.unlockFocus();
        cachedImages.addElement(image);
    }
}

public static void cacheImageNamed(String imageName) {
    NSImage image = (NSImage)NSImage.imageNamed(imageName);
    if (image == null) {
	NSSystem.log("Bad image to cache " + imageName);
    } else {
	cacheImage(image);
    }
}

}


