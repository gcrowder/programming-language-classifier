// SKTDrawDocument.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;
import java.util.*;
import java.lang.*;
import java.io.*;

public class SKTDrawDocument extends NSDocument {

    public static final String DrawDocumentType = "Apple Sketch Graphic Format";

    private static final String GraphicsListKey = "GraphicsList";
    private static final String DrawDocumentVersionKey = "DrawDocumentVersion";
    private static final int CurrentDrawDocumentVersion = 1;
    private static final String PrintInfoKey = "PrintInfo";

    private NSMutableArray _graphics;

    public SKTDrawDocument() {
        super();
        _graphics = new NSMutableArray();
    }

    // In Java constructors are not inherited.  We need to implement these babies even though we have nothing useful to add.
    public SKTDrawDocument(String file, String type) {
        super(file, type);
    }

    public SKTDrawDocument(java.net.URL url, String type) {
        super(url, type);
    }

    protected void finalize() {
        NSNotificationCenter.defaultCenter().removeObserver(this);
    }

    public void makeWindowControllers() {
        SKTDrawWindowController myController = new SKTDrawWindowController();
        addWindowController(myController);
    }

    public NSDictionary drawDocumentDictionaryForGraphics(NSArray graphics) {
        NSMutableDictionary doc = new NSMutableDictionary();
        int i, c = graphics.count();
        NSMutableArray graphicDicts = new NSMutableArray();

        for (i=0; i<c; i++) {
            graphicDicts.addObject(((SKTGraphic)graphics.objectAtIndex(i)).propertyListRepresentation());
        }
        doc.setObjectForKey(graphicDicts,GraphicsListKey);
        Integer version = new Integer(CurrentDrawDocumentVersion);
        doc.setObjectForKey(version.toString(), DrawDocumentVersionKey);
        doc.setObjectForKey(NSArchiver.archivedDataWithRootObject(printInfo()), PrintInfoKey);

        return doc;
    }

    public NSData drawDocumentDataForGraphics(NSArray graphics) {
        // NSStringReference and NSMutableStringReference have two potential uses.  They can be used to solve certain performance problems by preventing morphing of large strings when crossing the Java-ObjC bridge or they can be used to get access to the NSString API (such as encoding conversion API).  We pay a price for this convenience since we build a java.lang.String and then copy it all into an NSMutableStringReference, but boy it's a lot more convenient...
        NSDictionary doc = drawDocumentDictionaryForGraphics(graphics);
        String plistStr = NSPropertyListSerialization.stringFromPropertyList(doc);
        NSMutableStringReference mutStrRef = new NSMutableStringReference();
        NSData data;

        mutStrRef.setString(plistStr);
        data = mutStrRef.dataUsingEncoding(NSStringReference.ASCIIStringEncoding, false);
        
        return data;
    }

    public NSDictionary drawDocumentDictionaryFromData(NSData data) {
        // NSStringReference and NSMutableStringReference have two potential uses.  They can be used to solve certain performance problems by preventing morphing of large strings when crossing the Java-ObjC bridge or they can be used to get access to the NSString API (such as encoding conversion API).  We pay a price for this convenience since we build a java.lang.String and then copy it all into an NSMutableStringReference, but boy it's a lot more convenient...
        NSStringReference strRef = new NSStringReference(data, NSStringReference.ASCIIStringEncoding);
        NSDictionary doc = (NSDictionary)NSPropertyListSerialization.propertyListFromString(strRef.string());
        return doc;
    }

    public NSMutableArray graphicsFromDrawDocumentDictionary(NSDictionary doc) {
        NSArray graphicDicts = (NSArray) doc.objectForKey(GraphicsListKey);
        int i, c = graphicDicts.count();
        NSMutableArray graphics = new NSMutableArray();	

        for (i=0; i<c; i++) {
            graphics.addObject(SKTGraphic.graphicWithPropertyListRepresentation((NSDictionary)graphicDicts.objectAtIndex(i)));
        }

        return graphics;
    }

    public NSRect boundsForGraphics(NSArray graphics){
        NSMutableRect rect = new NSMutableRect(0,0,0,0);

        int i, c = graphics.count();
        for (i = 0; i < c; i++) {
            if (i == 0) {
                rect = new NSMutableRect (((SKTGraphic) graphics.objectAtIndex(i)).bounds());
            } else {
                rect = new NSMutableRect (rect.rectByUnioningRect( ((SKTGraphic) graphics.objectAtIndex(i)).bounds()));
            }
        }
        return rect;
    }

    public NSRect drawingBoundsForGraphics(NSArray graphics){
        NSMutableRect rect = new NSMutableRect (0,0,0,0);

        int i, c = graphics.count();
        for (i = 0; i < c; i++) {
            if (i == 0) {
                rect = new NSMutableRect (((SKTGraphic) graphics.objectAtIndex(i)).drawingBounds());
            } else {
                rect = new NSMutableRect ( rect.rectByUnioningRect( ((SKTGraphic) graphics.objectAtIndex(i)).drawingBounds()));
            }
        }
        return rect;
    }

    public NSData  TIFFRepresentationForGraphics (NSArray graphics ) {
        NSRect bounds = drawingBoundsForGraphics(graphics);
        NSImage image;
        NSData tiffData;
        int i = graphics.count();
        NSGraphicsContext currentContext;
        SKTGraphic g;
        
        if (bounds.isEmpty()) {
            return null;
        }
        image = new NSImage(bounds.size());
        image.setFlipped(true);
        image.lockFocus();
        // Get the context AFTER we lock focus
        currentContext = NSGraphicsContext.currentContext();
        NSAffineTransform transform = NSAffineTransform.transform();
        transform.translateXYBy(-bounds.x(), -bounds.y());
        transform.concat();

        while (i-- > 0) {
            // The only reason a graphic knows what view it is drawing in is so that it can draw differently when being created or edited or selected.  A nil view means to draw in the standard way.
            g = (SKTGraphic)graphics.objectAtIndex(i);
            currentContext.saveGraphicsState();
            NSBezierPath.clipRect(g.drawingBounds());
            g.drawInViewIsSelected(null,false);
            currentContext.restoreGraphicsState();
        }
        image.unlockFocus();
        tiffData = image.TIFFRepresentation();
        return tiffData;
    }

    public NSData PDFRepresentationForGraphics(NSArray graphics) {
        NSRect bounds = drawingBoundsForGraphics(graphics);

        SKTRenderingView view = new SKTRenderingView(new NSRect (0.0f, 0.0f, bounds.maxX(), bounds.maxY()), graphics);
        NSWindow tempWnd = new NSWindow(new NSRect (0.0f, 0.0f, bounds.maxX(), bounds.maxY()), NSWindow.BorderlessWindowMask, NSWindow.NonRetained, false);
        NSPrintInfo printInfo = printInfo();
        NSMutableData pdfData = new NSMutableData();
        NSPrintOperation printOp;

        tempWnd.contentView().addSubview(view);
        printOp = NSPrintOperation.PDFOperationWithViewInsideRect(view, bounds, pdfData, printInfo);
        printOp.setShowPanels(false);

        if (printOp.runOperation()) {
        } else {
            pdfData = null;
        }

        return pdfData;
    }

    public NSData dataRepresentationOfType(String type) {
        if (type.equals(DrawDocumentType)) {
            return this.drawDocumentDataForGraphics(this.graphics());
        } else if (type.equals(NSPasteboard.TIFFPboardType)) {
            return this.TIFFRepresentationForGraphics(this.graphics());
        } else if (type.equals(NSPasteboard.PDFPboardType)) {
            return this.PDFRepresentationForGraphics(this.graphics());
        } else {
            return null;
        }
    }

    public boolean loadDataRepresentation(NSData data, String type) {
        if (type.equals(DrawDocumentType)) {
            NSDictionary doc = this.drawDocumentDictionaryFromData(data);
            this.setGraphics(this.graphicsFromDrawDocumentDictionary(doc));

            data = (NSData)doc.objectForKey(PrintInfoKey);
            if (data != null) {
                NSPrintInfo printInfo = (NSPrintInfo)NSUnarchiver.unarchiveObjectWithData(data);
                if (printInfo != null) {
                    this.setPrintInfo(printInfo);
                }
            }

            if (this.undoManager() != null) {
                this.undoManager().removeAllActions();
            }

            return true;
        } else {
            return false;
        }
    }

    public void updateChangeCount(int changeType) {
        // This clears the undo stack whenever we load or save.
        super.updateChangeCount(changeType);
        if (changeType == NSDocument.ChangeCleared) {
            if (this.undoManager() != null) {
                this.undoManager().removeAllActions();
            }
        }
    }

    public NSWindow appropriateWindowForDocModalOperations() {
        NSArray wcs = this.windowControllers();
        int i, c = wcs.count();
        NSWindow docWindow = null;
        
        for (i=0; i<c; i++) {
            docWindow = ((NSWindowController)(wcs.objectAtIndex(i))).window();
            if (docWindow != null) {
                break;
            }
        }
        return docWindow;
    }

    public NSSize documentSize() {
        NSPrintInfo printInfo = this.printInfo();
        NSMutableSize paperSize = new NSMutableSize(printInfo.paperSize());
        paperSize.setWidth(paperSize.width() - (printInfo.leftMargin() + printInfo.rightMargin()));
        paperSize.setHeight(paperSize.height() - (printInfo.topMargin() + printInfo.bottomMargin()));
        return paperSize;
    }
    
    public void printShowingPrintPanel(boolean flag){
        NSSize paperSize = this.documentSize();

        NSRect tempRect = new NSRect (0.0f, 0.0f, paperSize.width(), paperSize.height());

        SKTRenderingView view = new SKTRenderingView(tempRect, this.graphics());
        NSWindow tempWnd = new NSWindow(tempRect, NSWindow.BorderlessWindowMask, NSWindow.NonRetained, false);
        tempWnd.contentView().addSubview(view);

        NSPrintOperation printOp = NSPrintOperation.printOperationWithView(view, printInfo());
        NSWindow docWindow = this.appropriateWindowForDocModalOperations();

        printOp.setShowPanels(flag);
        printOp.setCanSpawnSeparateThread(true);
        if (docWindow != null) {
            printOp.runModalOperation(docWindow, null, null, null);
        } else {
            printOp.runOperation();
        }
    }


    public void setPrintInfo(NSPrintInfo printInfo){
        if (undoManager() != null) {
            Class arrClass[] = {NSPrintInfo.class};
            Object args[] = {this.printInfo()};
            undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setPrintInfo", arrClass), args);
            undoManager().setActionName(NSBundle.localizedString("Change Print Info","Change Print Info", "UndoStrings"));
        }
        super.setPrintInfo(printInfo);
        
        NSArray controllers = this.windowControllers();
        int i, c = controllers.count();
        for (i=0; i<c; i++) {
            ((SKTDrawWindowController)(controllers.objectAtIndex(i))).setUpGraphicView();
        }
    }

    public NSArray graphics() {
        return _graphics;
    }

    public void setGraphics(NSMutableArray graphics) {
        int i = _graphics.count();
        while (i-- > 0) {
            removeGraphicAtIndex(i);
        }
        i = graphics.count();
        while (i-- > 0) {
            insertGraphicAtIndex((SKTGraphic) graphics.objectAtIndex(i),0);
        }
    }

    public void invalidateGraphic(SKTGraphic graphic) {
        NSArray windowControllers = windowControllers();

        if (windowControllers != null) {
            int i, c = windowControllers.count();
            for (i=0; i<c; i++) {
                ((SKTDrawWindowController)windowControllers.objectAtIndex(i)).invalidateGraphic(graphic);
            }
        }
    }

    public void insertGraphicAtIndex(SKTGraphic graphic, int index) {
        if (undoManager() != null) {
            Class arrClass[] = {Integer.TYPE};
            Object args[] = {new Integer(index)};
            undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("removeGraphicAtIndex", arrClass), args);
        }
        _graphics.insertObjectAtIndex(graphic,index);
        graphic.setDocument(this);
        invalidateGraphic(graphic);
    }

    public void removeGraphicAtIndex(int index) {
        Object graphic = _graphics.objectAtIndex(index);
        if (undoManager() != null) {
            Class arrClass[] = {SKTGraphic.class, Integer.TYPE};
            Object args[] = {graphic, new Integer(index)};
            undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("insertGraphicAtIndex", arrClass), args);
        }
        _graphics.removeObjectAtIndex(index);
        invalidateGraphic((SKTGraphic) graphic);
    }

    public void removeGraphic(SKTGraphic graphic) {
        int index = _graphics.indexOfIdenticalObject(graphic);
        if (index != NSArray.NotFound) {
            removeGraphicAtIndex(index);
        }
    }

    public void moveGraphicToIndex(SKTGraphic graphic, int newIndex){
        int curIndex = _graphics.indexOfIdenticalObject(graphic);
        if (curIndex != newIndex) {
            if (undoManager() != null) {
                Class arrClass[] = {SKTGraphic.class, Integer.TYPE};
                Object args[] = {graphic, new Integer(curIndex > newIndex ? curIndex+1 : curIndex)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("moveGraphicToIndex", arrClass), args);
            }
            if (curIndex < newIndex) {
                newIndex --;
            }
            _graphics.removeObjectAtIndex(curIndex);
            _graphics.insertObjectAtIndex(graphic,newIndex);
            invalidateGraphic(graphic);
        }
    }

    // Scripting support

    // graphics and setGraphics: are already implemented above.

    public void addInGraphics(SKTGraphic graphic) {
        this.insertGraphicAtIndex(graphic, this.graphics().count());
    }

    public void insertInGraphicsAtIndex(SKTGraphic graphic, int index) {
        this.insertGraphicAtIndex(graphic, index);
    }

    public void removeFromGraphicsAtIndex(int index) {
        this.removeGraphicAtIndex(index);
    }

    public void replaceInGraphicsAtIndex(SKTGraphic graphic, int index) {
        this.removeGraphicAtIndex(index);
        this.insertGraphicAtIndex(graphic, index);
    }

    public NSArray graphicsWithClass(Class theClass) {
        NSArray graphics = this.graphics();
        NSMutableArray result = new NSMutableArray();
        int i, c = graphics.count();
        SKTGraphic curGraphic;

        if (theClass != null) {
            for (i=0; i<c; i++) {
                curGraphic = (SKTGraphic)(graphics.objectAtIndex(i));
                if (theClass.isInstance(curGraphic)) {
                    result.addObject(curGraphic);
                }
            }
        }
        return result;
    }

    public NSArray rectangles() {
        return this.graphicsWithClass(SKTRectangle.class);
    }

    public NSArray circles() {
        return this.graphicsWithClass(SKTCircle.class);
    }

    public NSArray lines() {
        return this.graphicsWithClass(SKTLine.class);
    }

    public NSArray textAreas() {
        return this.graphicsWithClass(SKTTextArea.class);
    }

    public NSArray images() {
        return this.graphicsWithClass(SKTImage.class);
    }

    public void setRectangles(NSArray rects) {
        // We won't allow wholesale setting of these subset keys.
        // throw new NSException(NSKeyValue.OperationNotSupportedForKeyException);
    }

    public void addInRectangles(SKTGraphic graphic) {
        this.addInGraphics(graphic);
    }

    public void insertInRectanglesAtIndex(SKTGraphic graphic, int index) {
        // MF:!!! This is not going to be ideal.  If we are being asked to, say, "make a new rectangle at after rectangle 2", we will be after rectangle 2, but we may be after some other stuff as well since we will be asked to insertInRectangles:atIndex:3...
        NSArray rects = this.rectangles();
        if (index == rects.count()) {
            this.addInGraphics(graphic);
        } else {
            NSArray graphics = this.graphics();
            int newIndex = graphics.indexOfIdenticalObject(rects.objectAtIndex(index));
            if (newIndex != NSArray.NotFound) {
                this.insertGraphicAtIndex(graphic, newIndex);
            } else {
                // Shouldn't happen.
                // throw new NSException("NSRangeException");
            }
        }
    }

    public void removeFromRectanglesAtIndex(int index) {
        NSArray rects = this.rectangles();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(rects.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void replaceInRectanglesAtIndex(SKTGraphic graphic, int index) {
        NSArray rects = this.rectangles();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(rects.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
            this.insertGraphicAtIndex(graphic, newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void setCircles(NSArray rects) {
        // We won't allow wholesale setting of these subset keys.
        // throw new NSException(NSKeyValue.OperationNotSupportedForKeyException);
    }

    public void addInCircles(SKTGraphic graphic) {
        this.addInGraphics(graphic);
    }

    public void insertInCirclesAtIndex(SKTGraphic graphic, int index) {
        // MF:!!! This is not going to be ideal.  If we are being asked to, say, "make a new rectangle at after rectangle 2", we will be after rectangle 2, but we may be after some other stuff as well since we will be asked to insertInCircles:atIndex:3...
        NSArray circles = this.circles();
        if (index == circles.count()) {
            this.addInGraphics(graphic);
        } else {
            NSArray graphics = this.graphics();
            int newIndex = graphics.indexOfIdenticalObject(circles.objectAtIndex(index));
            if (newIndex != NSArray.NotFound) {
                this.insertGraphicAtIndex(graphic, newIndex);
            } else {
                // Shouldn't happen.
                // throw new NSException("NSRangeException");
            }
        }
    }

    public void removeFromCirclesAtIndex(int index) {
        NSArray circles = this.circles();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(circles.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void replaceInCirclesAtIndex(SKTGraphic graphic, int index) {
        NSArray circles = this.circles();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(circles.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
            this.insertGraphicAtIndex(graphic, newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void setLines(NSArray rects) {
        // We won't allow wholesale setting of these subset keys.
        // throw new NSException(NSKeyValue.OperationNotSupportedForKeyException);
    }

    public void addInLines(SKTGraphic graphic) {
        this.addInGraphics(graphic);
    }

    public void insertInLinesAtIndex(SKTGraphic graphic, int index) {
        // MF:!!! This is not going to be ideal.  If we are being asked to, say, "make a new rectangle at after rectangle 2", we will be after rectangle 2, but we may be after some other stuff as well since we will be asked to insertInLines:atIndex:3...
        NSArray lines = this.lines();
        if (index == lines.count()) {
            this.addInGraphics(graphic);
        } else {
            NSArray graphics = this.graphics();
            int newIndex = graphics.indexOfIdenticalObject(lines.objectAtIndex(index));
            if (newIndex != NSArray.NotFound) {
                this.insertGraphicAtIndex(graphic, newIndex);
            } else {
                // Shouldn't happen.
                // throw new NSException("NSRangeException");
            }
        }
    }

    public void removeFromLinesAtIndex(int index) {
        NSArray lines = this.lines();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(lines.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void replaceInLinesAtIndex(SKTGraphic graphic, int index) {
        NSArray lines = this.lines();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(lines.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
            this.insertGraphicAtIndex(graphic, newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void setTextAreas(NSArray rects) {
        // We won't allow wholesale setting of these subset keys.
        // throw new NSException(NSKeyValue.OperationNotSupportedForKeyException);
    }

    public void addInTextAreas(SKTGraphic graphic) {
        this.addInGraphics(graphic);
    }

    public void insertInTextAreasAtIndex(SKTGraphic graphic, int index) {
        // MF:!!! This is not going to be ideal.  If we are being asked to, say, "make a new rectangle at after rectangle 2", we will be after rectangle 2, but we may be after some other stuff as well since we will be asked to insertInTextAreas:atIndex:3...
        NSArray textAreas = this.textAreas();
        if (index == textAreas.count()) {
            this.addInGraphics(graphic);
        } else {
            NSArray graphics = this.graphics();
            int newIndex = graphics.indexOfIdenticalObject(textAreas.objectAtIndex(index));
            if (newIndex != NSArray.NotFound) {
                this.insertGraphicAtIndex(graphic, newIndex);
            } else {
                // Shouldn't happen.
                // throw new NSException("NSRangeException");
            }
        }
    }

    public void removeFromTextAreasAtIndex(int index) {
        NSArray textAreas = this.textAreas();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(textAreas.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void replaceInTextAreasAtIndex(SKTGraphic graphic, int index) {
        NSArray textAreas = this.textAreas();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(textAreas.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
            this.insertGraphicAtIndex(graphic, newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void setImages(NSArray rects) {
        // We won't allow wholesale setting of these subset keys.
        // throw new NSException(NSKeyValue.OperationNotSupportedForKeyException);
    }

    public void addInImages(SKTGraphic graphic) {
        this.addInGraphics(graphic);
    }

    public void insertInImagesAtIndex(SKTGraphic graphic, int index) {
        // MF:!!! This is not going to be ideal.  If we are being asked to, say, "make a new rectangle at after rectangle 2", we will be after rectangle 2, but we may be after some other stuff as well since we will be asked to insertInImages:atIndex:3...
        NSArray images = this.images();
        if (index == images.count()) {
            this.addInGraphics(graphic);
        } else {
            NSArray graphics = this.graphics();
            int newIndex = graphics.indexOfIdenticalObject(images.objectAtIndex(index));
            if (newIndex != NSArray.NotFound) {
                this.insertGraphicAtIndex(graphic, newIndex);
            } else {
                // Shouldn't happen.
                // throw new NSException("NSRangeException");
            }
        }
    }

    public void removeFromImagesAtIndex(int index) {
        NSArray images = this.images();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(images.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    public void replaceInImagesAtIndex(SKTGraphic graphic, int index) {
        NSArray images = this.images();
        NSArray graphics = this.graphics();
        int newIndex = graphics.indexOfIdenticalObject(images.objectAtIndex(index));
        if (newIndex != NSArray.NotFound) {
            this.removeGraphicAtIndex(newIndex);
            this.insertGraphicAtIndex(graphic, newIndex);
        } else {
            // Shouldn't happen.
            // throw new NSException("NSRangeException");
        }
    }

    // The following "indicesOf..." methods are in support of scripting.  They allow more flexible range and relative specifiers to be used with the different graphic keys of a SKTDrawDocument.
    // The scripting engine does not know about the fact that the "rectangles" key is really just a subset of the "graphics" key, so script code like "rectangles from circle 1 to line 4" don't make sense to it.  But Sketch does know and can answer such questions itself, with a little work.
    public NSArray indicesOfObjectsByEvaluatingRangeSpecifier(NSRangeSpecifier rangeSpec) {
        String key = rangeSpec.key();

        if (key.equals("graphics") || key.equals("rectangles") || key.equals("circles") || key.equals("lines") || key.equals("textAreas") || key.equals("images")) {
            // This is one of the keys we might want to deal with.
            NSScriptObjectSpecifier startSpec = rangeSpec.startSpecifier();
            NSScriptObjectSpecifier endSpec = rangeSpec.endSpecifier();
            String startKey = ((startSpec != null) ? startSpec.key() : null);
            String endKey = ((endSpec != null) ? endSpec.key() : null);
            NSArray graphics = this.graphics();

            if ((startSpec == null) && (endSpec == null)) {
                // We need to have at least one of these...
                return null;
            }
            if (graphics.count() == 0) {
                // If there are no graphics, there can be no match.  Just return now.
                return new NSArray();
            }

            if (((startSpec == null) || startKey.equals("graphics") || startKey.equals("rectangles") || startKey.equals("circles") || startKey.equals("lines") || startKey.equals("textAreas") || startKey.equals("images")) && ((endSpec == null) || endKey.equals("graphics") || endKey.equals("rectangles") || endKey.equals("circles") || endKey.equals("lines") || endKey.equals("textAreas") || endKey.equals("images"))) {
                int startIndex;
                int endIndex;

                // The start and end keys are also ones we want to handle.

                // The strategy here is going to be to find the index of the start and stop object in the full graphics array, regardless of what its key is.  Then we can find what we're looking for in that range of the graphics key (weeding out objects we don't want, if necessary).

                // First find the index of the first start object in the graphics array
                if (startSpec != null) {
                    Object startObject = startSpec.objectsByEvaluatingSpecifier();
                    if (startObject instanceof NSArray) {
                        if (((NSArray)startObject).count() == 0) {
                            startObject = null;
                        } else {
                            startObject = ((NSArray)startObject).objectAtIndex(0);
                        }
                    }
                    if (startObject == null) {
                        // Oops.  We could not find the start object.
                        return null;
                    }
                    startIndex = graphics.indexOfIdenticalObject(startObject);
                    if (startIndex == NSArray.NotFound) {
                        // Oops.  We couldn't find the start object in the graphics array.  This should not happen.
                        return null;
                    }
                } else {
                    startIndex = 0;
                }

                // Now find the index of the last end object in the graphics array
                if (endSpec != null) {
                    Object endObject = endSpec.objectsByEvaluatingSpecifier();
                    if (endObject instanceof NSArray) {
                        int endObjectsCount = ((NSArray)endObject).count();
                        if (endObjectsCount == 0) {
                            endObject = null;
                        } else {
                            endObject = ((NSArray)endObject).objectAtIndex(endObjectsCount-1);
                        }
                    }
                    if (endObject == null) {
                        // Oops.  We could not find the end object.
                        return null;
                    }
                    endIndex = graphics.indexOfIdenticalObject(endObject);
                    if (endIndex == NSArray.NotFound) {
                        // Oops.  We couldn't find the end object in the graphics array.  This should not happen.
                        return null;
                    }
                } else {
                    endIndex = graphics.count() - 1;
                }

                if (endIndex < startIndex) {
                    // Accept backwards ranges gracefully
                    int temp = endIndex;
                    endIndex = startIndex;
                    startIndex = temp;
                }

                // Now startIndex and endIndex specify the end points of the range we want within the graphics array.
                // We will traverse the range and pick the objects we want.
                // We do this by getting each object and seeing if it actually appears in the real key that we are trying to evaluate in.
                NSMutableArray result = new NSMutableArray();
                boolean keyIsGraphics = key.equals("graphics");
                NSArray rangeKeyObjects = (keyIsGraphics ? null : (NSArray)NSKeyValue.valueForKey(this, key));
                Object curObj;
                int curKeyIndex, i;

                for (i=startIndex; i<=endIndex; i++) {
                    if (keyIsGraphics) {
                        result.addObject(new Integer(i));
                    } else {
                        curObj = graphics.objectAtIndex(i);
                        curKeyIndex = rangeKeyObjects.indexOfIdenticalObject(curObj);
                        if (curKeyIndex != NSArray.NotFound) {
                            result.addObject(new Integer(curKeyIndex));
                        }
                    }
                }
                return result;
            }
        }
        return null;
    }

    public NSArray indicesOfObjectsByEvaluatingRelativeSpecifier(NSRelativeSpecifier relSpec) {
        String key = relSpec.key();

        if (key.equals("graphics") || key.equals("rectangles") || key.equals("circles") || key.equals("lines") || key.equals("textAreas") || key.equals("images")) {
            // This is one of the keys we might want to deal with.
            NSScriptObjectSpecifier baseSpec = relSpec.baseSpecifier();
            String baseKey = ((baseSpec != null) ? baseSpec.key() : null);
            NSArray graphics = this.graphics();
            int relPos = relSpec.relativePosition();

            if (baseSpec == null) {
                // We need to have one of these...
                return null;
            }
            if (graphics.count() == 0) {
                // If there are no graphics, there can be no match.  Just return now.
                return new NSArray();
            }
            
            if (baseKey.equals("graphics") || baseKey.equals("rectangles") || baseKey.equals("circles") || baseKey.equals("lines") || baseKey.equals("textAreas") || baseKey.equals("images")) {
                int baseIndex;

                // The base key is also one we want to handle.

                // The strategy here is going to be to find the index of the base object in the full graphics array, regardless of what its key is.  Then we can find what we're looking for before or after it.
                
                // First find the index of the first or last base object in the graphics array
		// Base specifiers are to be evaluated within the same container as the relative specifier they are the base of.  That's this document.
                Object baseObject = baseSpec.objectsByEvaluatingWithContainers(this);
                if (baseObject instanceof NSArray) {
                    int baseCount = ((NSArray)baseObject).count();
                    if (baseCount == 0) {
                        baseObject = null;
                    } else {
                        if (relPos == NSRelativeSpecifier.RelativeBefore) {
                            baseObject = ((NSArray)baseObject).objectAtIndex(0);
                        } else {
                            baseObject = ((NSArray)baseObject).objectAtIndex(baseCount-1);
                        }
                    }
                }
                if (baseObject == null) {
                    // Oops.  We could not find the base object.
                    return null;
                }
                                
                baseIndex = graphics.indexOfIdenticalObject(baseObject);
                if (baseIndex == NSArray.NotFound) {
                    // Oops.  We couldn't find the base object in the graphics array.  This should not happen.
                    return null;
                }

                // Now baseIndex specifies the base object for the relative spec in the graphics array.
		// We will start either right before or right after and look for an object that matches the type we want.
                // We do this by getting each object and seeing if it actually appears in the real key that we are trying to evaluate in.
                NSMutableArray result = new NSMutableArray();
                boolean keyIsGraphics = key.equals("graphics");
                NSArray relKeyObjects = (keyIsGraphics ? null : (NSArray)NSKeyValue.valueForKey(this, key));
                Object curObj;
                int curKeyIndex, graphicCount = graphics.count();

                if (relPos == NSRelativeSpecifier.RelativeBefore) {
                    baseIndex--;
                } else {
                    baseIndex++;
                }
                while ((baseIndex >= 0) && (baseIndex < graphicCount)) {
                    if (keyIsGraphics) {
                        result.addObject(new Integer(baseIndex));
                        break;
                    } else {
                        curObj = graphics.objectAtIndex(baseIndex);
                        curKeyIndex = relKeyObjects.indexOfIdenticalObject(curObj);
                        if (curKeyIndex != NSArray.NotFound) {
                            result.addObject(new Integer(curKeyIndex));
                            break;
                        }
                    }
                    if (relPos == NSRelativeSpecifier.RelativeBefore) {
                        baseIndex--;
                    } else {
                        baseIndex++;
                    }
                }

                return result;
            }
        }
        return null;
    }
    
    public NSArray indicesOfObjectsByEvaluatingObjectSpecifier(NSScriptObjectSpecifier specifier) {
        // We want to handle some range and relative specifiers ourselves in order to support such things as "graphics from circle 3 to circle 5" or "circles from graphic 1 to graphic 10" or "circle before rectangle 3".
	// Returning null from this method will cause the specifier to try to evaluate itself using its default evaluation strategy.
	
        if (specifier instanceof NSRangeSpecifier) {
            return this.indicesOfObjectsByEvaluatingRangeSpecifier((NSRangeSpecifier)specifier);
        } else if (specifier instanceof NSRelativeSpecifier) {
            return this.indicesOfObjectsByEvaluatingRelativeSpecifier((NSRelativeSpecifier)specifier);
        }


        // If we didn't handle it, return null so that the default object specifier evaluation will do it.
        return null;
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
