/*
	AwardView.java
	Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
	Author: Ali Ozer

	View which creates and draws (just for printing purposes) the 
        BlastApp certificate of completion.
*/
/*
 IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
 consideration of your agreement to the following terms, and your use, installation, 
 modification or redistribution of this Apple software constitutes acceptance of these 
 terms.  If you do not agree with these terms, please do not use, install, modify or 
 redistribute this Apple software.
 
 In consideration of your agreement to abide by the following terms, and subject to these 
 terms, Apple grants you a personal, non-exclusive license, under Apple’s copyrights in 
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

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;

class AwardView extends NSView {

public NSTextField nameField;
private int numLevels;
private int score;
private float textDrawingLocation;
private static NSBitmapImageRep helicopterBitmap;

public AwardView(NSRect rect) {
    super(rect);
}

protected boolean getUserName() {
    String name = NSSystem.currentFullUserName();
    if (name == null) name = NSSystem.currentUserName();

    if (nameField == null) {	// Haven't loaded the nib yet...
        if (!NSApplication.loadNibNamed("UserName.nib", this)) {
            return false;
	}
    }

    nameField.setStringValue(name != null ? name : "");
    nameField.selectText(null);

    if (NSApplication.sharedApplication().runModalForWindow(nameField.window()) == 0) {
        return false;
    }

    return true;
}

public void nameOK(NSControl sender) {
    NSApplication.sharedApplication().stopModalWithCode(1);
    nameField.window().orderOut(null);
}

public void nameCancel(NSControl sender) {
    NSApplication.sharedApplication().stopModalWithCode(0);
    nameField.window().orderOut(null);
}

public static void makeNewAward(int nLevels, int sc) {

    NSRect rect = new NSRect(0f, 0f, 500f, 500f);
    AwardView award = new AwardView(rect);

    if (!award.getUserName()) return;

    if (helicopterBitmap == null) cacheHelicopterBitmap();	// We do this explicitly as it doesn't work to do it while actually printing...

    NSPrintInfo printInfo = NSPrintInfo.sharedPrintInfo();
    NSWindow win = new NSWindow(rect, NSWindow.BorderlessWindowMask, NSWindow.NonRetained, false);
    win.contentView().addSubview(award);

    printInfo.setVerticallyCentered(true);
    printInfo.setHorizontallyCentered(true);
    printInfo.setVerticalPagination(NSPrintInfo.FitPagination);
    printInfo.setHorizontalPagination(NSPrintInfo.FitPagination);

    award.setLevels(nLevels, sc);
    award.print(null);
}

public void setLevels(int nLevels, int sc) {
    numLevels = nLevels;
    score = sc;
}

/* This method pretty much assumes the helicopter image is there... (Otherwise the game could not be played at all!)
*/
private static void cacheHelicopterBitmap() {
    NSImage helicopter = new NSImage(NSBundle.mainBundle().pathForResource("helicopter", "tiff"), true);	// true = by reference
    helicopter.setCacheDepthMatchesImageDepth(true);

    NSArray reps = helicopter.representations();

    // Assume that the first rep is the color one...
    helicopter.lockFocusOnRepresentation((NSImageRep)reps.objectAtIndex(0));
    helicopterBitmap = new NSBitmapImageRep(new NSRect(0f, 0f, helicopter.size().width(), helicopter.size().height() / 4f));
    helicopter.unlockFocus();
}

/* Draws a line of text and updates the next valid position to draw at... (the position is kept in "textDrawingLocation")
*/
public void drawLineOfText(String txt, float size) {
    NSMutableAttributedString attrStr = new NSMutableAttributedString(txt);
    NSRange range = new NSRange(0, attrStr.length());
    float viewWidth = bounds().width();
    float width;

    /* Find a good size... */
    do {
	/* Attributes that are not set use the well-known defaults (black, no underline, etc) */
	NSFont font = NSFont.fontWithNameAndSize("Times-Roman", size);
	attrStr.addAttributeInRange(NSAttributedString.FontAttributeName, font, range);
	width = NSGraphics.sizeOfAttributedString(attrStr).width();
        if (width < viewWidth * 0.9f) break;
	size = size - 2f;
    } while (size > 10f);
	
    NSGraphics.drawAttributedString(attrStr, new NSPoint((viewWidth - width) / 2f, textDrawingLocation));
    textDrawingLocation += (size * 1.3f + 4f);
}

public void drawRect(NSRect rect) {
    NSMutableRect hRect = new NSMutableRect();
    NSMutableRect sRect = new NSMutableRect();
    NSRect bounds = bounds();
    float loc;

    NSAffineTransform transform = new NSAffineTransform();
    transform.translateXYBy(0f, bounds.height());
    transform.scaleXYBy(1f, -1f);
    transform.concat();	// Flip the coord system (so it's unflipped); more convenient for the images

    sRect.setSize(helicopterBitmap.size());
    hRect.setWidth((float)Math.floor(bounds.width() * 0.6f));
    hRect.setHeight((float)Math.floor((hRect.width() / sRect.width()) * sRect.height()));
    hRect.setX((bounds.width() - hRect.width()) / 2f);
    hRect.setY(bounds.height() - hRect.height() * 1.5f);
    helicopterBitmap.drawInRect(hRect);

    sRect.setX(10f);
    sRect.setY(10f);
    helicopterBitmap.drawInRect(sRect);

    sRect.setX(bounds.maxX() - 10f - sRect.width());
    helicopterBitmap.drawInRect(sRect);

    sRect.setY(bounds.maxY() - 10f - sRect.height());
    helicopterBitmap.drawInRect(sRect);

    sRect.setX(10f); 
    helicopterBitmap.drawInRect(sRect);

    transform.concat();	// Go back to default (flipped) coord system...
	
    textDrawingLocation = bounds.height() - hRect.y() + 28f;

    drawLineOfText("Certificate of Achievement", 28f);
    drawLineOfText("awarded to", 20f);
    drawLineOfText(nameField.stringValue(), 48f);
    drawLineOfText("for valiantly completing the " + numLevels + " treacherous levels of", 20f);
    drawLineOfText("BlastApp", 28f);
    drawLineOfText("with a total score of " + score, 24f);

    try {
	NSGregorianDateFormatter dateFormatter = new NSGregorianDateFormatter("%A, %B %e, %Y", true);
        drawLineOfText(dateFormatter.stringForObjectValue(new NSGregorianDate()), 20f);
    } catch (NSFormatter.FormattingException e) {
    }

    NSGraphics.frameRect(bounds);
}

// Otherwise the text drawing (with NSGraphics.drawAttributedString() doesn't work quite right...)

public boolean isFlipped() {
    return true;
}

}

