/*
        MultiplePageView.java
        Copyright (c) 1995-2005 by Apple Computer, Inc., all rights reserved.
        Author: Yves Arrouye
	Java translation of original code by Ali Ozer

        View which holds all the pages together in the multiple-page case
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

public class MultiplePageView extends NSView {
    protected NSPrintInfo printInfo;
    protected NSColor lineColor, marginColor;
    protected int numPages;

    public MultiplePageView() {
        super();
        setupInitialState();
    }
    
    public MultiplePageView(NSRect rect) {
        super(rect);
        setupInitialState();
    }

    protected void setupInitialState() {
        numPages = 0;
        setLineColor(NSColor.lightGrayColor());
        setMarginColor(NSColor.whiteColor());
        setPrintInfo(NSPrintInfo.sharedPrintInfo());
    }
    
    public boolean isFlipped() {
        return true;
    }

    public boolean isOpaque() {
        return true;
    }

    public void updateFrame() {
        if (superview() != null) {
            NSMutableRect rect = new NSMutableRect();

            rect.setSize(printInfo.paperSize());
            rect.setHeight(rect.height() * numPages);
            if (numPages > 1) {
                rect.setHeight(rect.height() + pageSeparatorHeight() * (numPages - 1));
            }
            rect.setSize(convertSizeToView(rect.size(), superview()));
            
            setFrame(rect);
        }
    }

    public void setPrintInfo(NSPrintInfo anObject) {
        if (printInfo != anObject) {
            try {
                printInfo = (NSPrintInfo) anObject.clone();
            } catch (Exception e) {
                return;
            }
            updateFrame();
            setNeedsDisplay(true);
        }
    }

    public NSPrintInfo printInfo() {
        return printInfo;
    }

    public void setNumberOfPages(int num) {
        if (numPages != num) {
            NSRect oldFrame = frame();
            NSRect newFrame;

            numPages = num;
            updateFrame();
            newFrame = frame();

            if (newFrame.height() > oldFrame.height()) {
                setNeedsDisplay(new NSRect(oldFrame.x(), oldFrame.maxY(), oldFrame.width(), newFrame.maxY() - oldFrame.maxY()));
            }
        }
    }

    public int numberOfPages() {
        return numPages;
    }

    public float pageSeparatorHeight() {
        return 5f;
    }

    public NSSize documentSizeInPage() {
        NSMutableSize paperSize = new NSMutableSize(printInfo.paperSize());

        paperSize.setWidth(paperSize.width() - printInfo.leftMargin() - printInfo.rightMargin());
        paperSize.setHeight(paperSize.height() - printInfo.topMargin() - printInfo.bottomMargin());

        return paperSize;
    }
    
    public NSRect documentRectForPageNumber(int pageNumber) {
        NSMutableRect rect = new NSMutableRect(pageRectForPageNumber(pageNumber));

        rect.setX(rect.x() + printInfo.leftMargin());
        rect.setY(rect.y() + printInfo.topMargin());

        rect.setSize(documentSizeInPage());

        return rect;
    }

    public NSRect pageRectForPageNumber(int pageNumber) {
        NSMutableRect rect = new NSMutableRect();

        rect.setSize(printInfo.paperSize());
        rect.setOrigin(frame().origin());
        rect.setY(rect.y() + (rect.height() + pageSeparatorHeight()) * pageNumber);

        return rect;
    }

    public void setLineColor(NSColor color) {
        if (color != lineColor) {
            try {
                lineColor = (NSColor) color.clone();
            } catch (Exception e) {
                return;
            }
            setNeedsDisplay(true);
        }
    }

    public NSColor lineColor() {
        return lineColor;
    }

    public void setMarginColor(NSColor color) {
        if (color != marginColor) {
            try {
                marginColor = (NSColor) color.clone();
            } catch (Exception e) {
                return;
            }
            setNeedsDisplay(true);
        }
    }

    public NSColor marginColor() {
        return marginColor;
    }

    public void drawRect(NSRect rect) {
        if (NSGraphicsContext.currentContext().isDrawingToScreen()) {
            NSSize paperSize = printInfo.paperSize();
            int firstPage = (int) (rect.y() / (paperSize.height() + pageSeparatorHeight()));
            int lastPage = (int) (rect.maxY() / (paperSize.height() + pageSeparatorHeight()));
            int cnt;

            marginColor.set();
            NSBezierPath.fillRect(rect);

            lineColor.set();
            for (cnt = firstPage; cnt <= lastPage; ++cnt) {
                NSMutableRect docRect = new NSMutableRect(documentRectForPageNumber(cnt));
                docRect.insetRect(-1f, -1f);
                NSBezierPath.strokeRect(docRect);
            }

            if (superview() instanceof NSClipView) {
                NSColor backgroundColor = ((NSClipView) superview()).backgroundColor();
                
                backgroundColor.set();
                for (cnt = firstPage; cnt <= lastPage; ++cnt) {
                    NSRect pageRect = pageRectForPageNumber(cnt);
                    NSBezierPath.fillRect(new NSRect(pageRect.x(), pageRect.maxY(), pageRect.width(), pageSeparatorHeight()));
                }
            }
        }
    }

    // Printing support.

    public boolean knowsPageRange(NSMutableRange proposed) {
	if (proposed != null) {
	    proposed.setLocation(1);	// ??? 0 or 1
	    proposed.setLength(numberOfPages());
        }
	return true;
    }
    
    public NSRect rectForPage(int page) {
        return documentRectForPageNumber(page - 1);
    }
}
