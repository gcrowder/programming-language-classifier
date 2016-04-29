/*
        InputIndicator.java
        Copyright (c) 1990-2004, Apple Computer, Inc., all rights reserved.
        Author: Ali Ozer

        Subclass of NSView for displaying which key from the numeric pad is down
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

import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;

public class InputIndicator extends NSView {

protected boolean highlighted[] = new boolean[Game.NumCommands];

public InputIndicator(NSRect rect) {
    super(rect);
    allocateGState();
}

public void drawRect(NSRect rects) {
    NSImage image = (NSImage)NSImage.imageNamed("keypad");
    if (image != null) image.compositeToPoint(new NSPoint(), NSImage.CompositeSourceOver);
}

protected NSRect rectFor(int cmd) {
    float width = bounds().width() / 4f;
    float height = bounds().height() / 5f;
    float xLoc = 0f, yLoc = 0f;

    switch (cmd) {
        case Game.GoUpRightCommand:		xLoc = 2 * width; yLoc = 3 * height; break;
        case Game.GoUpCommand:			xLoc =     width; yLoc = 3 * height; break;
        case Game.GoUpLeftCommand:		yLoc = 3 * height; break;
        case Game.GoRightCommand:		xLoc = 2 * width; yLoc = 2 * height; break;
        case Game.StopCommand:			xLoc =     width; yLoc = 2 * height; break;
        case Game.GoLeftCommand:		yLoc = 2 * height; break;
        case Game.GoDownRightCommand:		xLoc = 2 * width; yLoc =     height; break;
        case Game.GoDownCommand:		xLoc =     width; yLoc =     height; break;
        case Game.GoDownLeftCommand:		yLoc =     height; break;
        case Game.FireCommand:			width *= 2; break;
        default:				return null;
    }

    return new NSRect(xLoc, yLoc, width, height).rectByMakingIntegral();
}

public void turnOn(int cmd) {
    if (!highlighted[cmd]) {
        NSRect rect = rectFor(cmd);
        if (rect != null) {
            rect = rect.rectByInsettingRect(1f, 1f);
            lockFocus();
            NSColor.whiteColor().set();
            NSBezierPath.strokeRect(rect);
            unlockFocus();
            highlighted[cmd] = true;
        }
    }
}

public void turnOff(int cmd) {
    if (highlighted[cmd]) {
        NSRect rect = rectFor(cmd);
        if (rect != null) {
            NSImage image = (NSImage)NSImage.imageNamed("keypad");
            lockFocus();
            if (image != null) image.compositeToPointFromRect (rect.origin(), rect, NSImage.CompositeSourceOver);
            unlockFocus();
            highlighted[cmd] = false;
        }
    }
}

}
