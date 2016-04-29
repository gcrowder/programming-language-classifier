// SKTGridPanelController.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.lang.*;


public class SKTGridPanelController extends NSWindowController {

    private static SKTGridPanelController _sharedGridPanelController = null;

    private NSButton snapsToGridCheckbox;
    private NSButton showsGridCheckbox;
    private NSSlider gridSpacingSlider;
    private NSColorWell gridColorWell;
    private SKTGridView gridView;
    private SKTGraphicView _inspectingGraphicView;

    public static SKTGridPanelController sharedGridPanelController() {

        if (_sharedGridPanelController == null) {
            _sharedGridPanelController = new SKTGridPanelController();
        }
        return _sharedGridPanelController;
    }

    public SKTGridPanelController() {
        super("GridPanel");
        this.setWindowFrameAutosaveName("GridPanel");
    }

    protected void finalize() {
        NSNotificationCenter.defaultCenter().removeObserver(this);
    }

    public void updatePanel() {
        if (isWindowLoaded()) {
            boolean hasGraphicView = ((_inspectingGraphicView == null) ? false : true);
            snapsToGridCheckbox.setState((snapsToGrid() ? 1 : 0));	
            showsGridCheckbox.setState((showsGrid() ? 1 : 0));
            gridSpacingSlider.setIntValue((int)gridSpacing());
            gridColorWell.setColor(gridColor());
            snapsToGridCheckbox.setEnabled(hasGraphicView);
            showsGridCheckbox.setEnabled(hasGraphicView);
            gridSpacingSlider.setEnabled(hasGraphicView);
            gridColorWell.setEnabled(hasGraphicView);
            gridView.setNeedsDisplay(true);
        }
    }

    public void setMainWindow(NSWindow mainWindow){
        NSWindowController controller = ((mainWindow != null) ? (NSWindowController)mainWindow.windowController() : null);

        if ((controller != null)  && (controller.getClass() == SKTDrawWindowController.class)) {
            _inspectingGraphicView =((SKTDrawWindowController) controller).graphicView();
        } else {
            _inspectingGraphicView = null;
        }
        updatePanel();
    }

    public void mainWindowChanged(NSNotification notification) {
        setMainWindow((NSWindow) notification.object());
    }

    public void mainWindowResigned(NSNotification notification) {
        setMainWindow(null);
    }

    public void windowDidLoad() {
        super.windowDidLoad();
        setMainWindow(NSApplication.sharedApplication().mainWindow());
        ((NSPanel)window()).setFloatingPanel(true);
        ((NSPanel)window()).setBecomesKeyOnlyIfNeeded(true);
        Class arrClass [] = {NSNotification.class};
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("mainWindowChanged", arrClass), NSWindow.WindowDidBecomeMainNotification, null);
        NSNotificationCenter.defaultCenter().addObserver(this, new NSSelector ("mainWindowResigned", arrClass), NSWindow.WindowDidResignMainNotification, null);
    }

    public boolean snapsToGrid() {
        return ((_inspectingGraphicView != null) ? _inspectingGraphicView.snapsToGrid() : false);
    }

    public boolean showsGrid() {
        return ((_inspectingGraphicView != null) ? _inspectingGraphicView.showsGrid() : false);
    }

    public float gridSpacing() {
        return ((_inspectingGraphicView != null) ? _inspectingGraphicView.gridSpacing() : 8);
    }

    public NSColor gridColor() {
        return ((_inspectingGraphicView != null) ? _inspectingGraphicView.gridColor() : NSColor.lightGrayColor());
    }

    public void snapsToGridCheckboxAction(NSButton sender) {
        if (_inspectingGraphicView != null) {
            _inspectingGraphicView.setSnapsToGrid((sender.state() == 1)? true:false);
        }
    }

    public void showsGridCheckboxAction(NSButton sender) {
        if (_inspectingGraphicView != null) {
            _inspectingGraphicView.setShowsGrid((sender.state()  == 1) ? true: false);
        }
    }

    public void gridSpacingSliderAction(NSSlider sender) {
        if (_inspectingGraphicView != null) {
            _inspectingGraphicView.setGridSpacing((float)sender.intValue());
        }
        gridView.setNeedsDisplay(true);
    }

    public void gridColorWellAction(NSColorWell sender) {
        if (_inspectingGraphicView != null) {
            _inspectingGraphicView.setGridColor(sender.color());
        }
        gridView.setNeedsDisplay(true);
    }

    public static void drawGridWithSettingsInRect(float spacing, NSColor color, NSRect rect, NSPoint gridOrigin) {
        int curLine, endLine;

        NSBezierPath gridPath = NSBezierPath.bezierPath();

        color.set();

        // Columns
        curLine = (int) Math.ceil((double) ((rect.x() - gridOrigin.x()) / spacing));
        endLine = (int) Math.floor((double) ((rect.maxX() - gridOrigin.x()) / spacing));

        for (; curLine<=endLine; curLine++) {
            gridPath.moveToPoint(new NSPoint((curLine* spacing) + gridOrigin.x(), rect.y()));
            gridPath.lineToPoint(new NSPoint((curLine* spacing) + gridOrigin.x(), rect.maxY()));
        }

        // Rows
        curLine = (int) Math.ceil ((double)  ((rect.y() - gridOrigin.y()) / spacing));
        endLine = (int) Math.floor ((double) ((rect.maxY() - gridOrigin.y()) / spacing));

        for (; curLine<=endLine; curLine++) {
            gridPath.moveToPoint(new NSPoint(rect.x(), (curLine * spacing) + gridOrigin.y()));
            gridPath.lineToPoint(new NSPoint(rect.maxX(), (curLine * spacing) + gridOrigin.y()));
        }

        gridPath.setLineWidth(0.0f);
        gridPath.stroke();
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
