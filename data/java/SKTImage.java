// SKTImage.java
// Sketch Example
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;

public class SKTImage extends SKTGraphic {

    private  NSImage _image;
    private  NSImage _cachedImage;
    private  boolean _flippedHorizontally;
    private  boolean _flippedVertically;

    public SKTImage () {
        _image = null;
        _cachedImage = null;
        _flippedHorizontally = false;
        _flippedVertically = false;
    }

    public Object clone() {
        SKTImage newObj = (SKTImage)super.clone();

        if (newObj != null) {
            newObj.setImage(this.image());
            newObj.setFlippedHorizontally(this.flippedHorizontally());
            newObj.setFlippedVertically(this.flippedVertically());
        }
        
        return newObj;
    }

    public void SKT_clearCachedImage() {
        _cachedImage = null;
    }

    public void setImage(NSImage image) {
        if (image != _image) {
            if (undoManager() != null) {
                Class arrClass[] = {NSImage.class};
                Object args[] = {_image};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setImage", arrClass), args);
            }
            _image = image;
            SKT_clearCachedImage();
            didChange();
        }
    }

    public NSImage image() {
        return _image;
    }

    public NSImage transformedImage() {
        if (_cachedImage == null) {
            NSRect bounds = bounds();
            NSImage image = image();
            
            if (image == null) {
                return null;
            }
            
            NSSize imageSize = image.size();

            if (bounds.size().isEqualToSize(imageSize)) {
                _cachedImage = _image;
            } else if (!bounds.isEqualToRect(new NSRect(0, 0, 0, 0))) {
                boolean flippedHorizontally = flippedHorizontally();
                boolean flippedVertically = flippedVertically();

                _cachedImage = new NSImage(bounds.size());
                if (!bounds.isEmpty()) {
                    _cachedImage.lockFocus();

                    if (flippedHorizontally || flippedVertically) {
                        // If the image needs flipping, we need to play some games with the transform matrix
                        NSAffineTransform transform = NSAffineTransform.transform();
                        transform.scaleXYBy((flippedHorizontally ? -1.0f : 1.0f),(flippedVertically ? -1.0f : 1.0f));
                        transform.translateXYBy((flippedHorizontally ? -bounds.width() : 0.0f),(flippedVertically ? -bounds.height() : 0.0f));
                        transform.concat();
                    }

                    image.bestRepresentationForDevice(null).drawInRect(new NSRect(0.0f,0.0f, bounds.width(), bounds.height()));
                    _cachedImage.unlockFocus();
                }
            }
        }
        return _cachedImage;
    }

    public void setFlippedHorizontally(boolean flag) {
        if (_flippedHorizontally != flag) {
            if (undoManager() != null) {
                Class arrClass[] = {Boolean.TYPE};
                Object args[] = {new Boolean(_flippedHorizontally)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setFlippedHorizontally", arrClass), args);
            }
            _flippedHorizontally = flag;
            SKT_clearCachedImage();
            didChange();
        }
    }

    public boolean flippedHorizontally() {
        return _flippedHorizontally;
    }

    public void setFlippedVertically(boolean flag) {
        if (_flippedVertically != flag) {
            if (undoManager() != null) {
                Class arrClass[] = {Boolean.TYPE};
                Object args[] = {new Boolean(_flippedVertically)};
                undoManager().registerUndoWithTargetAndArguments(this, new NSSelector("setFlippedVertically", arrClass), args);
            }
            _flippedVertically = flag;
            SKT_clearCachedImage();
            didChange();
        }
    }

    public boolean flippedVertically() {
        return _flippedVertically;
    }

    public void flipHorizontally() {
        setFlippedHorizontally((flippedHorizontally() ? false : true));
    }

    public void flipVertically() {
        setFlippedVertically((flippedVertically() ? false : true));
    }

    public void setBounds(NSRect bounds) {
        NSRect curBounds = bounds();
        if ((curBounds == null) || (!curBounds.size().isEqualToSize(bounds.size()))) {
            SKT_clearCachedImage();
        }
        super.setBounds(bounds);
    }

    public boolean drawsStroke() {
        // Never draw stroke.
        return false;
    }

    public boolean canDrawStroke() {
        // Never draw stroke.
        return false;
    }

    public void drawInViewIsSelected(SKTGraphicView view, boolean flag) {
        NSRect bounds = bounds();
        NSImage image;

        if (drawsFill()) {
            this.fillColor().set();
            NSBezierPath.fillRect(bounds);
        }
        image = transformedImage();
        if (image != null) {
            image.compositeToPoint(new NSPoint(bounds.x(),bounds.maxY()), NSImage.CompositeSourceOver);
        }
        super.drawInViewIsSelected(view, flag);
    }

    public void makeNaturalSize() {
        NSMutableRect bounds = new NSMutableRect(bounds());
        NSImage image = image();
        NSSize requiredSize = ((image != null) ? image.size() : new NSSize (10.0f, 10.0f));

        bounds.setSize(requiredSize);
        this.setBounds(bounds);
        setFlippedHorizontally(false);
        setFlippedVertically(false);
    }

    private static final String ImageContentsKey = "Image";
    private static final String FlippedHorizontallyKey = "FlippedHorizontally";
    private static final String FlippedVerticallyKey = "FlippedVertically";

    public NSMutableDictionary propertyListRepresentation() {
        NSMutableDictionary dict = super.propertyListRepresentation();
        dict.setObjectForKey(NSArchiver.archivedDataWithRootObject(image()),ImageContentsKey);
        dict.setObjectForKey((flippedHorizontally() ? "YES":"NO"),FlippedHorizontallyKey);
        dict.setObjectForKey((flippedVertically() ? "YES":"NO"),FlippedVerticallyKey);
        return dict;
    }

    public void loadPropertyListRepresentation(NSDictionary dict) {
        Object obj;

        super.loadPropertyListRepresentation(dict);

        obj = dict.objectForKey(ImageContentsKey);
        if (obj != null) {
            setImage((NSImage) NSUnarchiver.unarchiveObjectWithData((NSData)obj));
        }
        obj = dict.objectForKey(FlippedHorizontallyKey);
        if (obj != null) {
            setFlippedHorizontally(((String) obj).equals("YES"));
        }
        obj = dict.objectForKey(FlippedVerticallyKey);
        if (obj != null) {
            setFlippedVertically(((String) obj).equals("YES"));
        }
        _cachedImage = null;
    }

    public void setImageFile(String filePath) {
        filePath = NSPathUtilities.stringByStandardizingPath(filePath);
        filePath = NSPathUtilities.stringByExpandingTildeInPath(filePath);
        NSImage newImage = new NSImage(filePath, false);
        if (newImage != null) {
            this.setImage(newImage);
        }
    }

    public String imageFile() {
        // This is really a "write-only" attribute used for setting the image for an SKTImage shape from a script.  We don't remember the path so the accessor just returns an empty string.
        return "";
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
