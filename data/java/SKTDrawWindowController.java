// SKTDrawWindowController.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.lang.*;

public class SKTDrawWindowController extends NSWindowController {

    private SKTGraphicView graphicView;
    private NSMatrix toolButtons;

    public SKTDrawWindowController() {
        super("DrawWindow");
        window();
    }

    protected void finalize() {
        NSNotificationCenter.defaultCenter().removeObserver(this);
    }

    public NSSize documentSize() {
        if (this.document() != null) {
            return ((SKTDrawDocument)this.document()).documentSize();
        } else {
            NSPrintInfo printInfo = NSPrintInfo.sharedPrintInfo();
            NSMutableSize paperSize = new NSMutableSize(printInfo.paperSize());
            paperSize.setWidth(paperSize.width() - (printInfo.leftMargin() + printInfo.rightMargin()));
            paperSize.setHeight(paperSize.height() - (printInfo.topMargin() + printInfo.bottomMargin()));
            return paperSize;
        }
    }

    public void setUpGraphicView() {
        NSSize paperSize = this.documentSize();
        graphicView.setFrameSize(paperSize);
        graphicView.setNeedsDisplay(true);
    }

    public void selectedToolDidChange(NSNotification notification) {
        // Just set the correct cursor
        Class theClass = SKTToolPaletteController.sharedToolPaletteController().currentGraphicClass();
        NSCursor theCursor = null;
        if (theClass != null) {
            // All this reflection nastiness is a substitute for the lack of dynamic dispatch of static methods in Java.  We want to call the creationCursor static method that is implemented in the particular SKTGraphic subclass we are dealing with.  Reflection is the only way to go for this.
            // An alternative would be to have the SKTGraphic class provide a registry for cursors for its subclasses (eg setCreationCursorForSubclass(NSCursor cursor, Class subclass)) and make the creationCursor method in SKTGraphic take a Class as an argument (eg creationCursorForSubclass(Class subclass)).
            java.lang.reflect.Method curMethod = null;

            try {
                curMethod = theClass.getMethod("creationCursor", null);
            } catch (NoSuchMethodException e) {
                curMethod = null;
            }

            if (curMethod != null) {
                try {
                    theCursor = (NSCursor)curMethod.invoke(null, null);
                } catch (IllegalAccessException e) {
                    theCursor = null;
                } catch (java.lang.reflect.InvocationTargetException e) {
                    theCursor = null;
                }
            }
        }
        if (theCursor == null) {
            theCursor = NSCursor.arrowCursor();
        }
        graphicView.enclosingScrollView().setDocumentCursor(theCursor);
    }

    public void windowDidLoad() {
        NSScrollView enclosingScrollView;
        
        super.windowDidLoad();

        graphicView.setDrawWindowController(this);
        this.setUpGraphicView();

        enclosingScrollView = graphicView.enclosingScrollView();
        enclosingScrollView.setHasHorizontalRuler(true);
        enclosingScrollView.setHasVerticalRuler(true);
        enclosingScrollView.setBorderType(((NSInterfaceStyle.interfaceStyleForKey(null, enclosingScrollView) == NSInterfaceStyle.Windows95InterfaceStyle) ? NSView.BezelBorder : NSView.NoBorder));

        window().makeFirstResponder(graphicView);

        selectedToolDidChange(null);

        Class arrClass [] = {NSNotification.class};
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("selectedToolDidChange", arrClass), SKTToolPaletteController.SelectedToolDidChangeNotification, SKTToolPaletteController.sharedToolPaletteController());
    }

    public void setDocument(NSDocument document) {
        super.setDocument(document);
        setUpGraphicView();
    }

    public SKTGraphicView graphicView() {
        return graphicView;
    }

    public void invalidateGraphic(SKTGraphic graphic) {
        graphicView.invalidateGraphic(graphic);
    }

}

/*
 IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
 consideration of your agreement to the following terms, and your use, installation,
 modification or redistribution of this Apple software constitutes acceptance of these
 terms.  If you do not agree with these terms, please do not use, install, modify or
 redistribute this Apple software.

 In consideration of your agreement to abide by the following terms, and subject to these
 terms, Apple grants you a personal, non-exclusive license, under Apple's copyrights in
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
