/*
        ScalingScrollView.java
        Copyright (c) 1995-2005 by Apple Computer, Inc., all rights reserved.
        Author: Yves Arrouye
        Java translation of original code by Mike Ferris
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

/* For genstrings:
    NSLocalizedString(@"10%", @"Zoom popup entry")
    NSLocalizedString(@"25%", @"Zoom popup entry")
    NSLocalizedString(@"50%", @"Zoom popup entry")
    NSLocalizedString(@"75%", @"Zoom popup entry")
    NSLocalizedString(@"100%", @"Zoom popup entry")
    NSLocalizedString(@"128%", @"Zoom popup entry")
    NSLocalizedString(@"200%", @"Zoom popup entry")
    NSLocalizedString(@"400%", @"Zoom popup entry")
    NSLocalizedString(@"800%", @"Zoom popup entry")
    NSLocalizedString(@"1600%", @"Zoom popup entry")
*/
public class ScalingScrollView extends NSScrollView {
    
    protected NSPopUpButton _scalePopUpButton = null;
    protected float _scaleFactor = 1.0f;

    public ScalingScrollView (NSRect rect) {
        super(rect);
    }

    protected static final String MenuLabels[] = { "10%", "25%", "50%", "75%", "100%", "128%", "200%", "400%", "800%", "1600%" };
    protected static final float ScaleFactors[] = { 0.1f, 0.25f, 0.5f, 0.75f, 1f, 1.28f, 2f, 4f, 8f, 16f };
    protected static final int DefaultSelectedItem = 4;

    protected void makeScalePopUpButton () {
        if (_scalePopUpButton == null) {
            /* Create the pop up button. */
            _scalePopUpButton = new NSPopUpButton(new NSRect(0f, 0f, 1f, 1f), false);
            _scalePopUpButton.setBezelStyle(NSButtonCell.RegularSquareBezelStyle);

            /* Fill it with the scales. */
            int numberOfDefaultItems = MenuLabels.length;
            for (int i=0; i<numberOfDefaultItems; i++) {
                String label = NSBundle.localizedString(MenuLabels[i]);
                _scalePopUpButton.addItem(label);

                NSMenuItem item = (NSMenuItem) _scalePopUpButton.itemAtIndex(i);
                
                if (ScaleFactors[i] != 0f) {
                    Number factor = new Float(ScaleFactors[i]);
                    item.setRepresentedObject(factor);
                }
            }
            _scalePopUpButton.selectItemAtIndex(DefaultSelectedItem);

            /* Hook it up. */
            _scalePopUpButton.setTarget(this);
            _scalePopUpButton.setAction(new NSSelector("scalePopUpAction", new Class[] {getClass()}));

            /* Pick a suitable font. */
            _scalePopUpButton.setFont(NSFont.toolTipsFontOfSize(10f));

            /* Make sure the pop up is big enough to fit the cells. */
            _scalePopUpButton.sizeToFit();

            /* Don't ever let it become the first responder. */
            _scalePopUpButton.setRefusesFirstResponder(true);

            /* Put it in the scroll view. */
            addSubview(_scalePopUpButton);
        }
    }

    public void tile () {
        /* Let the superclass do most of the work. */
        super.tile();

        if (!hasHorizontalScroller()) {
            if (null != _scalePopUpButton) {
                _scalePopUpButton.removeFromSuperview();
                _scalePopUpButton = null;
            }
        } else {
            if (_scalePopUpButton == null) makeScalePopUpButton();

            NSScroller horizScroller = horizontalScroller();
            NSMutableRect horizScrollerFrame = new NSMutableRect(horizScroller.frame());
            NSRect incrementLineFrame = horizScroller.rectForPart(NSScroller.IncrementLine);
            NSMutableRect buttonFrame = new NSMutableRect(_scalePopUpButton.frame());

            /* Adjust the horizontal scroller size and set the button size and location. */
            buttonFrame.setX(horizScrollerFrame.x());
            buttonFrame.setHeight(incrementLineFrame.height());
            buttonFrame.setY(horizScrollerFrame.y() + incrementLineFrame.y());

            horizScrollerFrame.setWidth(horizScrollerFrame.width() - (buttonFrame.width() + 1.0f));
            horizScrollerFrame.setX(horizScrollerFrame.x() + (buttonFrame.width() + 1.0f));
            horizScroller.setFrame(horizScrollerFrame);

            _scalePopUpButton.setFrame(buttonFrame);
        }
    }

    public void drawRect(NSRect rect) {
        NSMutableRect verticalLineRect;

        super.drawRect(rect);

        if ((_scalePopUpButton != null)  && (_scalePopUpButton.superview() != null)) {
            verticalLineRect = new NSMutableRect(_scalePopUpButton.frame());
            verticalLineRect.setX(verticalLineRect.maxX());
            verticalLineRect.setWidth(1.0f);
            if (verticalLineRect.intersectsRect(rect)) {
                NSColor.blackColor().set();
                NSBezierPath.bezierPathWithRect(verticalLineRect).fill();
            }
        }
    }

    public void scalePopUpAction (Object sender) {
        Number selectedFactorObject = (Number) ((NSButtonCell) ((NSPopUpButton) sender).selectedCell()).representedObject();

        if (selectedFactorObject == null) {
            NSSystem.log("Setting arbitrary zoom factors is not yet supported.");
        } else {
            setScaleFactor(selectedFactorObject.floatValue());
        }
    }

    public float scaleFactor () {
        return _scaleFactor;
    }

    public void setScaleFactor (float newScaleFactor) {
        if (_scaleFactor != newScaleFactor) {
            NSView clipView = ((NSView)documentView()).superview();

            _scaleFactor = newScaleFactor;

            /* The frame must stay the same. */
            NSSize curDocFrameSize = clipView.frame().size();

            /* The new bounds will be frame divided by scale factor. */
            NSSize newDocBoundsSize = new NSSize(curDocFrameSize.width() / newScaleFactor, curDocFrameSize.height() / newScaleFactor);

            clipView.setBoundsSize(newDocBoundsSize);
        }
    }


    public void setHasHorizontalScroller (boolean flag) {
        if (flag == false) {
            setScaleFactor(1.0f);
        }
        super.setHasHorizontalScroller(flag);
    }
}
