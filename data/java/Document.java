/*
        Document.java
        Copyright (c) 1995-2005 by Apple Computer, Inc., all rights reserved.
        Author: Yves Arrouye
        Java translation of original code by Ali Ozer

        Document object for TextEdit. Needs to be switched over to the NSDocument object.

	Note: documents put themselves into a global array of documents when
	they are created, and remove themselves from this list when they're
	about to be deleted. This is necessary at the moment because on the
	Objective-C side of the world some objects are holding weak references
	to the document objects, and if we do not do that the Objective-C
	proxies will be autoreleased, provoking the death of the application
	on a `message sent to freed object' error.
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

import java.util.*;
import java.net.*;
import java.text.MessageFormat;
import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;

public class Document {
    public static final int UnknownStringEncoding = -1000;
    public static final int RichTextStringEncoding = -1001;
    public static final int RichTextWithGraphicsStringEncoding = -1002;
    public static final int HTMLStringEncoding = -1003;
    
    protected static NSPopUpButton encodingPopUpButton = null;
    protected static NSView theEncodingAccessory = null;
    protected static NSButton ignoreRichTextButton = null;
    protected static int[] availableEncodings = null;
    
    protected NSTextStorage textStorage;
    protected URL documentURL;		// If null, the document has never been saved.
    protected URL baseURL;		// If null, the document has never been saved.
    protected NSScrollView scrollView;		// ScrollView containing the document.

    protected NSPrintInfo printInfo;		// PrintInfo, used when hasMultiplePages is true.
    boolean hasMultiplePages;

    protected boolean isDocumentEdited;
    protected boolean isRichText;

    protected int documentEncoding;

    protected String potentialSaveDirectory = null;	// If non-null, path prefix where to save a document.

    protected NSUndoManager undoManager = null;
    
    private static NSMutableArray allDocuments = new NSMutableArray();
    
    private static NSPoint cascadePoint = new NSPoint(0f, 0f);
    private static String lastOpenSavePanelDir = null;

    private static NSMutableArray tabStopArray = null;
    private static float currentWidthOfTab = -1f;

    Document() {
        allDocuments.addObject(this);

        textStorage = new NSTextStorage();

        if (NSApplication.loadNibNamed("Document", this) == false) {
            NSSystem.log("Couldn't load Document.nib");
            return;
        }

        NSLayoutManager layoutManager = new NSLayoutManager();
        textStorage.addLayoutManager(layoutManager);
        layoutManager.setDelegate(this);

        setEncoding(UnknownStringEncoding); // setEncoding(Preferences.intValueForKey(Preferences.PlainTextEncodingForRead));

        setPrintInfo(NSPrintInfo.sharedPrintInfo());
        printInfo().setHorizontalPagination(NSPrintInfo.FitPagination);
        printInfo.setHorizontallyCentered(false);
        printInfo.setVerticallyCentered(false);

        // This gives us our first view.

        setHasMultiplePages(Preferences.booleanValueForKey(Preferences.ShowPageBreaks));

        // This ensures the first view gets set up correctly.

	setupInitialTextViewSharedState();

        window().setDelegate(this);

        if (hasMultiplePages()) {
            setViewSize(((MultiplePageView) scrollView.documentView()).pageRectForPageNumber(0).size());
        } else {
            int windowHeight = Preferences.intValueForKey(Preferences.WindowHeight);
            int windowWidth = Preferences.intValueForKey(Preferences.WindowWidth);

            NSFont font = (NSFont) Preferences.objectForKey(isRichText() ? Preferences.RichTextFont : Preferences.PlainTextFont);
            NSMutableSize size = new NSMutableSize();
            size.setHeight((float) Math.ceil(font.boundingRectForFont().size().height()) * windowHeight);
            size.setWidth(font.widthOfString("x"));
            if (size.width() == 0.0) size.setWidth(font.widthOfString(" "));
            if (size.width() == 0.0) size.setWidth(font.maximumAdvancement().width());
            size.setWidth((float) Math.ceil(size.width() * windowWidth + firstTextView().textContainer().lineFragmentPadding() * 2.0));

            setViewSize(size);
        }

        if (cascadePoint.equals(new NSPoint())) {
            NSRect frame = window().frame();
            cascadePoint = new NSPoint(frame.origin().x(), frame.maxY());
        }
        cascadePoint = window().cascadeTopLeftFromPoint(cascadePoint);

	NSNotificationCenter center = (NSNotificationCenter)NSNotificationCenter.defaultCenter();

        center.addObserver(this, new NSSelector("fixUpScrollViewBackgroundColor", new Class[] {getClass()}), NSColor.SystemColorsDidChangeNotification, null);
        center.addObserver(this, new NSSelector ("textStorageDidProcessEditing", new Class[] {getClass()}), NSTextStorage.TextStorageDidProcessEditingNotification, this.textStorage());

        potentialSaveDirectory = null;
    }

    Document(URL url, int encoding, boolean ignoreRTF, boolean ignoreHTML) {
        this();

        setDocumentURL(url);

        if (documentURL != null) {
            if (!TextReadWrite.loadFromURL(documentURL, this, encoding, ignoreRTF, ignoreHTML)) {
                setDocumentURL(null);
                throw new IllegalArgumentException("Cannot load " + documentURL);
            } else {
                NSDocumentController.sharedDocumentController().noteNewRecentDocumentURL(documentURL);
            }
            if (documentURL.getProtocol().equals("file")) {
                Document.setLastOpenSavePanelDirectory(NSPathUtilities.stringByDeletingLastPathComponent(NSPathUtilities.pathFromURL(documentURL)));
            }
        }

        firstTextView().setSelectedRange(new NSRange(0, 0));
        setDocumentEdited(false);	// This is set as a side-effect of calling textStorageDidProcessEditing
    }

    protected void finalize() throws Throwable {        
        documentURL = null;
        baseURL = null;
        printInfo = null;

        undoManager.removeAllActions();
        undoManager = null;
        
        super.finalize();
    }
    
    public void setupInitialTextViewSharedState() {
        NSTextView textView = firstTextView();

        textView.setUsesFontPanel(true);
        textView.setDelegate(this);
        textView.setAllowsUndo(true);
        setRichText(Preferences.booleanValueForKey(Preferences.RichText));
        setHyphenationFactor(0f);
    }

    // Default attributes for the plain or rich state

    public NSDictionary defaultTextAttributes(boolean forRichText) {
        NSMutableDictionary textAttributes = new NSMutableDictionary();
	if (forRichText) {
	    textAttributes.setObjectForKey(Preferences.objectForKey(Preferences.RichTextFont), NSAttributedString.FontAttributeName);
	} else {
            textAttributes.setObjectForKey(Preferences.objectForKey(Preferences.PlainTextFont), NSAttributedString.FontAttributeName);
	}
        textAttributes.setObjectForKey(NSParagraphStyle.defaultParagraphStyle(), NSAttributedString.ParagraphStyleAttributeName);
 	return textAttributes;
    }

    public NSTextView firstTextView() {
        return layoutManager().firstTextView();
    }

    public NSSize viewSize() {
        return scrollView.contentSize();
    }

    public void setViewSize(NSSize size) {
        NSWindow window = scrollView.window();
        NSRect origWindowFrame = window.frame();
        NSSize scrollViewSize = NSScrollView.frameSizeForContentSize(size, scrollView.hasHorizontalScroller(), scrollView.hasVerticalScroller(), scrollView.borderType());

        window.setContentSize(scrollViewSize);
        window.setFrameTopLeftPoint(new NSPoint(origWindowFrame.origin().x(), origWindowFrame.maxY()));
    }

    // The following method causes the text to be laid out in the foreground (approximately) up to the indicated character index.

    public void doForegroundLayoutToCharacterIndex(int loc) {
        int len;

        if (loc > 0 && (len = textStorage.length()) > 0) {
            NSRange glyphRange;

            if (loc >= len) loc = len - 1;

            // Find out which glyph index the desired character index corresponds to.
            
            glyphRange = layoutManager().glyphRangeForCharacterRange(new NSRange(loc, 1), null);
            
            if (glyphRange.location() > 0) {
                layoutManager().textContainerForGlyphAtIndex(glyphRange.location() - 1, null);	// This causes layout to be done.
            }
        }
    }

    public static URL cleanedUpURL(URL url) {
        if (url == null) {
            return null;
        } else if (!url.getProtocol().equals("file")) {
            return url;
        } else {
            String filename = NSPathUtilities.pathFromURL(url);
            String resolvedSymlinks = NSPathUtilities.stringByResolvingSymlinksInPath(filename);
            if (resolvedSymlinks != null && resolvedSymlinks.length() > 0) {
                String standardized = NSPathUtilities.stringByStandardizingPath(resolvedSymlinks);
                if (standardized != null && standardized.length() > 0) {
                    filename = standardized;
                } else {
                    filename = resolvedSymlinks;
                }
            }
            return NSPathUtilities.URLWithPath(filename);
        }
    }

    public void setDocumentURL(URL url) {
        if (url != null) {
            url = cleanedUpURL(url);
            documentURL = url;
            if (url.getProtocol().equals("file")) {
                window().setTitleWithRepresentedFilename(NSPathUtilities.pathFromURL(url));
            } else {
                window().setTitle(url.toString());
            }
        } else {
            String untitled = NSBundle.localizedString("Untitled", "Name of new, untitled document");
            if (isRichText()) {
                untitled = NSPathUtilities.stringByAppendingPathExtension(untitled, "rtf");
            }
            if (potentialSaveDirectory != null) {
                window().setTitleWithRepresentedFilename(NSPathUtilities.stringByAppendingPathComponent(potentialSaveDirectory, untitled));
            } else {
                window().setTitle(untitled);
            }

            documentURL = null;
        }
        baseURL = documentURL;
    }

    public void setBaseURL(URL url) {
        baseURL = url;
    }

    public URL documentURL() {
        return documentURL;
    }

    public void setPotentialSaveDirectory(String nm) {
        if (Preferences.booleanValueForKey(Preferences.OpenPanelFollowsMainWindow) == false) {
            return;
        }
        potentialSaveDirectory = nm;
    }

    public String potentialSaveDirectory() {
        if (Preferences.booleanValueForKey(Preferences.OpenPanelFollowsMainWindow) == false) {
            return NSSystem.homeDirectoryForUser(NSSystem.currentUserName());
        }
        return potentialSaveDirectory;
    }

    public void setDocumentEdited(boolean flag) {
        if (flag != isDocumentEdited) {
            isDocumentEdited = flag;
            window().setDocumentEdited(isDocumentEdited);
        }
    }

    public boolean isDocumentEdited() {
        return isDocumentEdited;
    }

    public NSTextStorage textStorage() {
        return textStorage;
    }

    public NSWindow window() {
        NSView textView = firstTextView();

        return textView != null ? textView.window() : null;
    }

    public NSLayoutManager layoutManager() {
        return (NSLayoutManager) textStorage.layoutManagers().objectAtIndex(0);
    }

    public void setPrintInfo(NSPrintInfo anObject) {
        if (printInfo != null) {
            if (printInfo == anObject || printInfo.dictionary().equals(anObject.dictionary())) {
                return;
            }
        }

        try {
            printInfo = (NSPrintInfo) anObject.clone();
        } catch (Exception e) {
            return;
        }
        
        if (hasMultiplePages()) {
            int cnt, numberOfPages = numberOfPages();
            MultiplePageView pagesView = (MultiplePageView) scrollView.documentView();
            NSArray textContainers = layoutManager().textContainers();

            pagesView.setPrintInfo(printInfo);

            for (cnt = 0; cnt < numberOfPages; ++cnt) {
                NSRect textFrame = pagesView.documentRectForPageNumber(cnt);
                NSTextContainer textContainer = (NSTextContainer) textContainers.objectAtIndex(cnt);

                textContainer.setContainerSize(textFrame.size());
                textContainer.textView().setFrame(textFrame);
            }
        }
    }

    public NSPrintInfo printInfo() {
        return printInfo;
    }

    /*
     * Multiple page related code.
     *
     */

    public int numberOfPages() {
        if (hasMultiplePages) {
            return ((MultiplePageView) scrollView.documentView()).numberOfPages();
        }

        return 1;
    }

    public boolean hasMultiplePages() {
        return hasMultiplePages;
    }

    public void addPage() {
        int numberOfPages = numberOfPages();
        MultiplePageView pagesView = (MultiplePageView) scrollView.documentView();
        NSSize textSize = pagesView.documentSizeInPage();
        NSTextContainer textContainer = new NSTextContainer(textSize);

        pagesView.setNumberOfPages(numberOfPages + 1);

        NSTextView textView = new NSTextView(pagesView.documentRectForPageNumber(numberOfPages), textContainer);
        textView.setHorizontallyResizable(false);
        textView.setVerticallyResizable(false);

        pagesView.addSubview(textView);
        layoutManager().addTextContainer(textContainer);
    }

    public void removePage() {
        int numberOfPages = numberOfPages();
        NSArray textContainers = layoutManager().textContainers();
        NSTextContainer lastContainer = (NSTextContainer) textContainers.objectAtIndex(textContainers.count() - 1);
        MultiplePageView pagesView = (MultiplePageView) scrollView.documentView();

        pagesView.setNumberOfPages(numberOfPages - 1);
        lastContainer.textView().removeFromSuperview();
        lastContainer.layoutManager().removeTextContainerAtIndex(textContainers.count() - 1);
    }

    public void setHasMultiplePages(boolean flag) {
        hasMultiplePages = flag;

        if (hasMultiplePages) {
            NSTextView textView = firstTextView();
            MultiplePageView pagesView = new MultiplePageView();

            scrollView.setDocumentView(pagesView);

            pagesView.setPrintInfo(printInfo());
            addPage();
            if (textView != null) {
                layoutManager().removeTextContainerAtIndex(0);
            }
            scrollView.setHasHorizontalScroller(true);
        } else {
            NSSize size = scrollView.contentSize();
            NSTextContainer textContainer = new NSTextContainer(new NSSize((float) size.width(), Float.MAX_VALUE));
            NSTextView textView = new NSTextView(new NSRect(0f, 0f, size.width(), size.height()), textContainer);

            layoutManager().insertTextContainerAtIndex(textContainer, 0);

            if (scrollView.documentView() instanceof MultiplePageView) {
                NSArray textContainers = layoutManager().textContainers();
                int cnt = textContainers.count();

                while (cnt-- > 1) {
                    layoutManager().removeTextContainerAtIndex(cnt);
                }
            }

            textContainer.setWidthTracksTextView(true);
            textContainer.setHeightTracksTextView(false);

            textView.setHorizontallyResizable(false);
            textView.setVerticallyResizable(true);

            textView.setAutoresizingMask(NSView.ViewWidthSizable);
            textView.setMinSize(size);
            textView.setMaxSize(new NSSize(Float.MAX_VALUE, Float.MAX_VALUE));

            scrollView.setDocumentView(textView);
            scrollView.setHasHorizontalScroller(false);
        }

        scrollView.window().makeFirstResponder(firstTextView());
    }

    /*
     * The following method is called at startup in response to a colors changed notification to fix up the color displayed in the
     * background area of the scrollview (actually clipview) when in "wrap to page" mode.
     *   The reason we need to get fancy here is to assure that the color on Windows is a blend of black and the "3D Objects" color
     * (which is what some Windows apps do). If the color was a regular system color, registering for the notification would not be
     * necessary.
     *
     */

    public void fixUpScrollViewBackgroundColor(NSNotification notification) {
        NSColor backgroundColor = null;

        if (NSInterfaceStyle.interfaceStyleForKey(NSInterfaceStyle.InterfaceStyleDefault, scrollView) == NSInterfaceStyle.Windows95InterfaceStyle) {
            backgroundColor = NSColor.controlColor().blendedColorWithFractionOfColor(0.5f, NSColor.blackColor());
        } else {
            backgroundColor = NSColor.controlColor();
        }

        scrollView.contentView().setBackgroundColor(backgroundColor);
    }

    public void setScrollView(Object anObject) {
        scrollView = (NSScrollView) anObject;
        
        scrollView.setHasVerticalScroller(true);
        scrollView.setHasHorizontalScroller(false);
        scrollView.contentView().setAutoresizesSubviews(true);
        fixUpScrollViewBackgroundColor(null);
        if (NSInterfaceStyle.interfaceStyleForKey(NSInterfaceStyle.InterfaceStyleDefault, scrollView) == NSInterfaceStyle.Windows95InterfaceStyle) {
            scrollView.setBorderType(NSView.BezelBorder);
        }
    }

    // Return (after loading, if necessary), the encoding accessory to be used in Open & Save panels...

    public static NSView encodingAccessory(int encoding, boolean includeDefaultItem, boolean enableIgnoreRichTextButton) {
        // Load the encoding accessory through the accessory class if needed.
        
        if (theEncodingAccessory == null) {
            EncodingAccessoryOwner owner = new EncodingAccessoryOwner();

            if (owner.loadEncodingAccessory()) {
                encodingPopUpButton = owner.encodingPopUpButton;
                ignoreRichTextButton = owner.ignoreRichTextButton;
                theEncodingAccessory = owner.encodingAccessory;
                encodingPopUpButton.menu().setAutoenablesItems(false);
            }
        }

        ignoreRichTextButton.setEnabled(enableIgnoreRichTextButton);
        setupEncodingPopUpButton(encodingPopUpButton, encoding, includeDefaultItem);

        return theEncodingAccessory;
    }

    public void removeAttachments() {
	NSTextStorage attrString = textStorage();
        int loc = 0;
        int end = attrString.length();

        attrString.beginEditing();
        while (loc < end) {
            NSMutableRange attachmentRange = new NSMutableRange();
            NSTextAttachment attachment = (NSTextAttachment) attrString.attributeAtIndex(NSAttributedString.AttachmentAttributeName, loc, attachmentRange, new NSRange(loc, end - loc));
            
            if (attachment != null) {	// If there is an attachment, make sure it is valid.
                char ch = attrString.stringReference().characterAtIndex(loc);

                if (ch == NSTextAttachment.AttachmentCharacter) {
                    attrString.replaceCharactersInRange(new NSRange(loc, 1), "");
		    end = attrString.length();
                } else {
                    ++loc;
                }
            } else {
                loc = attachmentRange.maxRange();
            }
        }

        attrString.endEditing();
    }

    public void setHyphenationFactor(float factor) {
        layoutManager().setHyphenationFactor(factor);
    }

    public float hyphenationFactor() {
        return layoutManager().hyphenationFactor();
    }

    public int encoding() {
        return documentEncoding;
    }
    
    public void setEncoding(int enc) {
        documentEncoding = enc;
    }
    
    public void setRichText(boolean flag) {
        NSTextView view = firstTextView();
        NSDictionary textAttributes;

        isRichText = flag;

        if (!isRichText) removeAttachments();

        view.setRichText(isRichText);
        view.setUsesRuler(isRichText);
        view.setImportsGraphics(isRichText);

   	textAttributes = defaultTextAttributes(isRichText);

        if (textStorage.length() != 0) {
            textStorage.setAttributesInRange(textAttributes, new NSRange(0, textStorage.length()));
        }

        view.setTypingAttributes(textAttributes);
    }

    public boolean isRichText() {
        return isRichText;
    }

    public void printDocumentUsingPrintPanel(boolean uiFlag) {
        NSPrintOperation op = NSPrintOperation.printOperationWithView((NSView) scrollView.documentView(), printInfo);
        op.setShowPanels(uiFlag);
        op.runOperation();
    }

    public void printDocument(Object sender) {
        printDocumentUsingPrintPanel(true);
    }
    
    public void toggleRich(Object sender) {
	int len = textStorage.length();
        if (isRichText && len > 0) {
	    NSMutableRange range = new NSMutableRange();
            NSDictionary attrs = (NSDictionary)textStorage.attributesAtIndex(0, range);
	    if (attrs == null || range.length() < len || !defaultTextAttributes(true).equals(attrs)) {
                int choice = NSAlertPanel.runAlert(NSBundle.localizedString("Make Plain Text", "Title of alert confirming Make Plain Text"), NSBundle.localizedString("Convert document to plain text? This will lose fonts, colors, and other text attribute settings.", "Message confirming Make Plain Text"), NSBundle.localizedString("OK", "OK"), NSBundle.localizedString("Cancel", "Cancel"), null);

                if (choice != NSAlertPanel.DefaultReturn) {
                    return;
                }
	    }
        }

        setRichText(!isRichText);
        setEncoding(UnknownStringEncoding);
        setDocumentEdited(true);
        setDocumentURL(null);
    }

    public void togglePageBreaks(Object sender) {
        setHasMultiplePages(!hasMultiplePages);
    }

    public void toggleHyphenation(Object sender) {
        setHyphenationFactor(hyphenationFactor() > 0f ? 0f : 0.9f);
        if (isRichText()) {
            setDocumentEdited(true);
        }
    }

    public void runPageLayout(Object sender) {
        try {
            NSPrintInfo tempPrintInfo = (NSPrintInfo) printInfo().clone();
            NSPageLayout pageLayout = NSPageLayout.pageLayout();
            int runResult = pageLayout.runModalWithPrintInfo(tempPrintInfo);

            if (runResult == NSPageLayout.PLOKButton) {
                setPrintInfo(tempPrintInfo);
            }
        } catch (Exception e) {
            ; // Just ignore the exception that may only be CloneNotSupportedException thrown by an old NativeObject
        }
    }

    public void revert(Object sender) {
        if (documentURL != null) {
            boolean revert = true;
            Object[] localizedArgs = {
                NSPathUtilities.lastPathComponent(NSPathUtilities.pathFromURL(documentURL))
            };

            if (isDocumentEdited()) {	// If not edited, we don't bother asking...
                revert = (NSAlertPanel.DefaultReturn == NSAlertPanel.runAlert(NSBundle.localizedString("Revert", "Title of alert confirming revert"), MessageFormat.format(NSBundle.localizedString("Revert to saved version of {0}?", "Message confirming revert of document."), localizedArgs), NSBundle.localizedString("OK", "OK"), NSBundle.localizedString("Cancel", "Cancel"), null));
            }
            if (revert) {
                if (!TextReadWrite.loadFromURL(documentURL, this, documentEncoding, Preferences.booleanValueForKey(Preferences.IgnoreRichText), Preferences.booleanValueForKey(Preferences.IgnoreHTML))) {
                    NSAlertPanel.runAlert(NSBundle.localizedString("Couldn't Revert", "Title of alert indicating file couldn't be reverted"), MessageFormat.format(NSBundle.localizedString("Could not revert to saved version of {0}.", "Message indicating document couldn't be reverted."), localizedArgs), NSBundle.localizedString("OK", "OK"), null, null);
                } else {
                    setDocumentEdited(false);
                    NSDocumentController.sharedDocumentController().noteNewRecentDocumentURL(documentURL);
                }
            }
        }
    }
    
    public void close(Object sender) {
        window().close();
    }

    public void saveTo(Object sender) {
        saveDocument(true, false);
    }

    public void saveAs(Object sender) {
        saveDocument(true, true);
    }

    public void save(Object sender) {
        saveDocument(false, true);
    }
    
    public static void setupEncodingPopUpButton(NSPopUpButton popup, int selectedEncoding, boolean includeDefaultItem) {
        boolean defaultItemIsIncluded = false;
        int cnt = popup.numberOfItems();

        if (cnt <= 1) {
            int[] encodings = supportedEncodings();
            int i;
            
            popup.removeAllItems();
            for (i = 0, cnt = encodings.length; i < cnt; ++i) {
                int encValue = encodings[i];
                popup.addItem(NSStringReference.localizedNameOfStringEncoding(encValue));
                popup.lastItem().setTag(encValue);
            }
        } else {
            while (!defaultItemIsIncluded && cnt-- > 0) {
                NSMenuItem item = (NSMenuItem) popup.itemAtIndex(cnt);
                if (item.tag() == UnknownStringEncoding) {
                    defaultItemIsIncluded = true;
                }
            }
        }

        if (includeDefaultItem && !defaultItemIsIncluded) {
            popup.insertItemAtIndex(NSBundle.localizedString("Automatic", "Encoding popup entry indicating automatic choice of encoding"), 0);
            popup.itemAtIndex(0).setTag(UnknownStringEncoding);
        } else if (!includeDefaultItem && defaultItemIsIncluded) {
            popup.removeItemAtIndex(0);
        }

        defaultItemIsIncluded = includeDefaultItem;
        
        cnt = popup.numberOfItems();
        while (cnt-- > 0) {
            NSMenuItem item = (NSMenuItem) popup.itemAtIndex(cnt);
            item.setEnabled(true);
            if (item.tag() == selectedEncoding) {
                popup.selectItemAtIndex(cnt);
            }
        }
    }

    // Allow reading the list of encodings to display in the popups from resource named Encodings.txt
    
    private static int[] supportedEncodings() {
        if (availableEncodings == null) {
            String str = NSBundle.mainBundle().pathForResource("Encodings", "txt");
            NSStringReference strRef;
            if (str != null && (strRef = new NSStringReference(NSPathUtilities.URLWithPath(str))) != null) {
                NSArray encodings = NSArray.componentsSeparatedByString(strRef.string(), ",");
                int numEncodings = encodings.count();
		int [] tmpAvailableEncodings = new int[numEncodings];
                int cnt, numValidEncodings = 0;
                for (cnt = 0; cnt < numEncodings; cnt++) {
		    try {
                        int encoding = (new Integer((String)(encodings.objectAtIndex(cnt)))).intValue();
                        if (!NSStringReference.localizedNameOfStringEncoding(encoding).equals("")) tmpAvailableEncodings[numValidEncodings++] = encoding;
		    } catch (Exception e) {
                    }
                }
                if (numValidEncodings == numEncodings) {
                    availableEncodings = tmpAvailableEncodings;
                } else {
                    availableEncodings = new int[numValidEncodings];
                    for (cnt = 0; cnt < numValidEncodings; cnt++) availableEncodings[cnt] = tmpAvailableEncodings[cnt];
                }
 	    }
	}
        if (availableEncodings == null) {
            availableEncodings = new int[] {
                NSStringReference.MacOSRomanStringEncoding,
                NSStringReference.WindowsCP1252StringEncoding,
                NSStringReference.UTF8StringEncoding,
                NSStringReference.NEXTSTEPStringEncoding,
                NSStringReference.ISOLatin1StringEncoding,
                NSStringReference.ISOLatin2StringEncoding,
                NSStringReference.ShiftJISStringEncoding,
                NSStringReference.JapaneseEUCStringEncoding,
                NSStringReference.UnicodeStringEncoding
            };
        }

        return availableEncodings;
    }

    public NSUndoManager undoManager() {
        if (undoManager == null) undoManager = new NSUndoManager();
        return undoManager;
    }

    protected static NSArray tabStopArrayForFontAndTabWidth(NSFont font, int tabWidth) {
        float charWidth;
        
        if (font.glyphIsEncoded(' ')) {
            charWidth = font.advancementForGlyph(' ').width();
        } else {
            charWidth = font.maximumAdvancement().width();
        }

        float widthOfTab = charWidth * tabWidth;

        if (tabStopArray == null) {
            tabStopArray = new NSMutableArray();
        }

        if (widthOfTab != currentWidthOfTab) {
            tabStopArray.removeAllObjects();
            for (int i = 1; i <= 100; ++i) {
                NSTextTab tab = new NSTextTab(NSTextTab.LeftTabStopType, widthOfTab * i);
                tabStopArray.addObject(tab);
            }
            currentWidthOfTab = widthOfTab;
        }

        return tabStopArray;
    }

    protected void fixUpTabs(NSRange editedRange) {
        NSStringReference string = textStorage.stringReference();
        if (string.length() > 0) {
            // Generally TextStorages attached to plain text TextViews only have one font.  But this is not always true.  To ensure the tabstops are uniform throughout the document we always base them on the font of the first character in the TextStorage.
            NSFont font = (NSFont)textStorage.attributeAtIndex(NSAttributedString.FontAttributeName, 0, null);

            // Substitute a screen font is the layout manager will do so for display.
            // ??? Printing will probably be an issue here...
            font = layoutManager().substituteFontForFont(font);
            
            if (font.isFixedPitch()) {
		NSMutableRange fixRange = new NSMutableRange(string.lineRangeForRange(editedRange));	/* The range to fix */

                if (fixRange.location() < string.length()) {
                    int tabWidth = Preferences.intValueForKey(Preferences.TabWidth);
                    NSArray desiredTabStops = Document.tabStopArrayForFontAndTabWidth(font, tabWidth);
                    NSMutableRange effRange = new NSMutableRange();	/* Longest effective range of current paraStyle */

                    // We will traverse the edited range by paragraphs fixing the paragraph styles
                    while (true) {
                        NSParagraphStyle paraStyle = (NSParagraphStyle)textStorage.attributeAtIndex(NSAttributedString.ParagraphStyleAttributeName, fixRange.location(), effRange, fixRange);

                        if (paraStyle == null) paraStyle = NSParagraphStyle.defaultParagraphStyle();

                        effRange.intersectRange(fixRange);

                        if (!paraStyle.tabStops().equals(desiredTabStops)) {
                            NSMutableParagraphStyle newStyle;	// Make sure we don't change stuff outside editedRange.
                            try {
                                newStyle = (NSMutableParagraphStyle)paraStyle.mutableClone();
                            } catch (Exception e) {
                                newStyle = new NSMutableParagraphStyle();
                                newStyle.setParagraphStyle(paraStyle);
                            }
                            newStyle.setTabStops(desiredTabStops);
                            textStorage.addAttributeInRange(NSAttributedString.ParagraphStyleAttributeName, newStyle, effRange);
                        }

                        if (effRange.maxRange() >= fixRange.maxRange()) break;	/* We're done */

                        fixRange.setLength(fixRange.maxRange() - effRange.maxRange());
                        fixRange.setLocation(effRange.maxRange());
                    }
                }
            }
        }
    }

    protected void textStorageDidProcessEditing(NSNotification notification) {

        // Mark the document as dirty.  We used to check this in textDidChange:, but this only catches changes initiated through the NSTextView.  With upcoming developments such as scriptability opening the door to the possibility that changes will happen without the view's involvement, it is now best to catch changes here at the NSTextStorage level.
        if (!isDocumentEdited()) {
            setDocumentEdited(true);
        }

        // Fix up the tabs so they are the correct width in the edited region
        if (!isRichText()) {
            fixUpTabs(textStorage.editedRange());
        }
    }

    // Text delegate methods.

    public boolean textViewClickedOnLinkAtIndex(NSTextView sender, Object value, int charIndex) {
        URL link;

        if (((String)value).startsWith("#")) {
            // Local marker link.  We can't handle this because the markers aren't recorded anywhere in the attributed string when loaded from the html.
            return false;
        }
        try {
            link = new URL(baseURL, (String)value);
        } catch (MalformedURLException e) {
            System.out.println("Failed to create URL from link string: " + value + " with base URL: " + baseURL);
            link = null;
        }
        if (link != null) {
            Document.openDocumentWithURL(link, UnknownStringEncoding, Preferences.booleanValueForKey(Preferences.IgnoreRichText), Preferences.booleanValueForKey(Preferences.IgnoreHTML));
        }

        return true;
    }

    public void textViewDoubleClickedCell(NSTextView view, NSTextAttachmentCell cell, NSRect rect) {
        boolean success = false;
        String name = cell.attachment().fileWrapper().filename();

        if (name != null && documentURL != null && !name.equals("") && documentURL.getProtocol().equals("file") && !NSPathUtilities.pathFromURL(documentURL).equals("")) {
            String fullPath = NSPathUtilities.stringByAppendingPathComponent(NSPathUtilities.pathFromURL(documentURL), name);
            success = NSWorkspace.sharedWorkspace().openFile(fullPath);
        }

        if (!success) {
            NSApplication.beep();
        }
    }

    public void textViewDraggedCell(NSTextView view, NSTextAttachmentCell cell, NSRect rect, NSEvent event) {
        boolean success = false;
        String name = cell.attachment().fileWrapper().filename();

        if (name != null && documentURL != null && !name.equals("") && documentURL.getProtocol().equals("file") && !NSPathUtilities.pathFromURL(documentURL).equals("")) {
            String fullPath = NSPathUtilities.stringByAppendingPathComponent(NSPathUtilities.pathFromURL(documentURL), name);
            NSImage image = null;

            if (cell instanceof NSCell) {
                image = ((NSCell) cell).image();
            }

            if (image == null) {
                image = NSWorkspace.sharedWorkspace().iconForFile(fullPath);
            }

            if (image != null) {
                NSSize cellSizeInBaseCoords = view.convertSizeToView(rect.size(), null);
                NSSize imageSize = image.size();
                NSPasteboard pasteboard = NSPasteboard.pasteboardWithName(NSPasteboard.DragPboard);
                NSMutableRect mrect = new NSMutableRect(rect);
                
                pasteboard.declareTypes((NSArray) new NSArray(NSPasteboard.FilenamesPboardType), null);
                pasteboard.setPropertyListForType((NSArray) new NSArray(fullPath), NSPasteboard.FilenamesPboardType);

                if (!cellSizeInBaseCoords.equals(imageSize)) {
                    NSPoint mouseDownLocation = view.convertPointFromView(event.locationInWindow(), null);

                    mrect.setX((float) (mouseDownLocation.x() - (imageSize.width() - Math.floor(imageSize.width() / 4.0))));
                    mrect.setY((float) (mouseDownLocation.y() - (imageSize.height() - Math.floor(imageSize.height() / 4.0))));
                } else {
                    mrect.setY(mrect.y() + mrect.height());
                }

                view.dragImage(image, mrect.origin(), new NSSize(), event, pasteboard, this, true);
                success = true;
            }
        }

        if (!success) {
            NSApplication.beep();
        }
    }

    public int draggingSourceOperationMaskForLocal(boolean flag) {
        return NSDraggingInfo.DragOperationGeneric | NSDraggingInfo.DragOperationCopy;
    }

    // Layout manager delegate methods.

    public void layoutManagerDidCompleteLayoutForTextContainer(NSLayoutManager layoutManager, NSTextContainer textContainer, boolean layoutFinishedFlag) {
        if (hasMultiplePages()) {
            NSArray containers = layoutManager.textContainers();

            if (!layoutFinishedFlag || textContainer == null) {
                NSTextContainer lastContainer = (NSTextContainer) containers.lastObject();

                if (textContainer == lastContainer || textContainer == null) {
                    addPage();
                }
            } else {
                int lastUsedContainerIndex = containers.indexOfObject(textContainer);
                int numContainers = containers.count();

                while (++lastUsedContainerIndex < numContainers) {
                    removePage();
                }
            }
        }
    }
    
    // Window delegate methods.

    // Set miniwindow image lazily (not strictly necessary)
    public void windowWillMiniaturize(NSNotification notification) {
	NSImage icon = NSWorkspace.sharedWorkspace().iconForFileType (isRichText() ? "rtf" : "txt");
	if (icon != null) window().setMiniwindowImage(icon);
    }

    public NSUndoManager windowWillReturnUndoManager(NSWindow window) {
        return undoManager();
    }

    public boolean windowShouldClose(Object sender) {
        return canCloseDocument();
    }

    public void windowWillClose(NSNotification notification) {
        NSWindow window = window();
        
        if (window != null) {
            NSNotificationCenter center = (NSNotificationCenter)NSNotificationCenter.defaultCenter();
            
            center.removeObserver(this, NSColor.SystemColorsDidChangeNotification, null);
            center.removeObserver(this, NSTextStorage.TextStorageDidProcessEditingNotification, textStorage());

            firstTextView().setDelegate(null);
            layoutManager().setDelegate(null);
            window.setDelegate(null);

            allDocuments.removeObject(this);
            textStorage = null;
            scrollView = null;
            undoManager = null;
        }
    }

    public boolean windowShouldZoom(NSWindow window, NSRect newFrame) {
        return true;
    }
    
    public NSRect windowWillUseStandardFrame(NSWindow window, NSRect defaultFrame) {
        NSMutableRect currentFrame = new NSMutableRect(window.frame());
        NSMutableRect standardFrame = new NSMutableRect();
        NSSize paperSize = printInfo.paperSize();
        NSSize aSize = NSScrollView.frameSizeForContentSize(paperSize, scrollView.hasHorizontalScroller(), scrollView.hasVerticalScroller(), scrollView.borderType());
        
        aSize = NSWindow.frameRectForContentRect(new NSRect(0f, 0f, aSize.width(), aSize.height()), window.styleMask()).size();

        standardFrame.setWidth(aSize.width());
        standardFrame.setHeight(aSize.height());
        standardFrame.setY(currentFrame.maxY() - standardFrame.height());
        standardFrame.setX(currentFrame.x());

        return standardFrame;
    }
    
    // Document opening code.

    public static void open(Object sender) {
        openWithEncodingAccessory(true);
    }

    public static void openWithEncodingAccessory(boolean flag) {
        NSOpenPanel panel = NSOpenPanel.openPanel();
        boolean ignoreRichPref = Preferences.booleanValueForKey(Preferences.IgnoreRichText);
        boolean ignoreHTMLPref = Preferences.booleanValueForKey(Preferences.IgnoreHTML);

        if (flag) {
            panel.setAccessoryView(encodingAccessory(Preferences.intValueForKey(Preferences.PlainTextEncodingForRead), true, true));

            // if the ignoreRichText and ignoreHTML preference values do not agree, then the initial
            // state of the ignore button in the panel should be "mixed" state, indicating
            // it will do the appropriate thing depending on the file selected...
           if (ignoreRichPref != ignoreHTMLPref) {
                ignoreRichTextButton.setAllowsMixedState(true);
                ignoreRichTextButton.setState(NSCell.MixedState);
            } else {
                if (ignoreRichTextButton.allowsMixedState()) ignoreRichTextButton.setAllowsMixedState(false);
                ignoreRichTextButton.setState(ignoreRichPref ? NSCell.OnState : NSCell.OffState);
            }
        }

        panel.setAllowsMultipleSelection(true);
        panel.setDirectory(Document.openSavePanelDirectory());

        if (panel.runModal() != 0) {
            NSArray urls = panel.URLs();
            int cnt, numURLs = urls.count();
                               
            for (cnt = 0; cnt < numURLs; ++cnt) {
                URL url = (URL)urls.objectAtIndex(cnt);

                if (ignoreRichTextButton.state() == NSCell.OffState) {
                    ignoreRichPref = ignoreHTMLPref = false;
                } else if (ignoreRichTextButton.state() == NSCell.OnState) {
                    ignoreRichPref = ignoreHTMLPref = true;
                } // Otherwise they both remain at their respective state...

                if (Document.openDocumentWithURL(url, flag ? encodingPopUpButton.selectedItem().tag() : UnknownStringEncoding, ignoreRichPref, ignoreHTMLPref) == null) {
                    
                    Object[] localizedArgs = {
                        url.getFile()
                    };

                    String alternate = (cnt + 1 == numURLs) ? null : NSBundle.localizedString("Abort", "Button allowing user to abort opening multiple files after one couldn't be opened");
                    int choice = NSAlertPanel.runAlert(NSBundle.localizedString("Open Failed", "Title of alert indicating file couldn't be opened"), MessageFormat.format(NSBundle.localizedString("Could not open file {0}.", "Message indicating file couldn't be opened; {0} is the filename."), localizedArgs), NSBundle.localizedString("OK", "OK"), alternate, null);

                    if (choice == NSPanel.CancelButton) {
                        break;
                    }
                }
            }
        }
    }
    
    public static Document openUntitled() {
        Document document = new Document(null, UnknownStringEncoding, false, false);
        document.setPotentialSaveDirectory(Document.openSavePanelDirectory());
        document.setDocumentURL(null);
        document.window().makeKeyAndOrderFront(null);
        return document;
    }

    public static Document openDocumentWithURL(URL url, int encoding, boolean ignoreRich, boolean ignoreHTML) {
        Document document = documentForURL(url);
        if (document == null) {
            try {
                document = new Document(url, encoding, ignoreRich, ignoreHTML);
            } catch (Exception e) {
                return null;
            }
        }
        document.doForegroundLayoutToCharacterIndex(Preferences.intValueForKey(Preferences.ForegroundLayoutToIndex));
        document.window().makeKeyAndOrderFront(null);
        return document;
    }

    protected static void setLastOpenSavePanelDirectory(String dir) {
        if (lastOpenSavePanelDir != dir) {
            lastOpenSavePanelDir = new String(dir);
        }
    }

    protected static String openSavePanelDirectory() {
        if (Preferences.booleanValueForKey(Preferences.OpenPanelFollowsMainWindow)) {
	    NSWindow win = NSApplication.sharedApplication().mainWindow();
            Document doc = (win == null) ? null : Document.documentForWindow(win);
            if (doc != null && doc.documentURL() != null && doc.documentURL.getProtocol().equals("file")) {
                return NSPathUtilities.stringByDeletingLastPathComponent(NSPathUtilities.pathFromURL(doc.documentURL()));
            } else if (doc != null && lastOpenSavePanelDir != null) {
                return lastOpenSavePanelDir;
            }
        } else if (lastOpenSavePanelDir != null) {
            return lastOpenSavePanelDir;
        }

        return NSSystem.homeDirectoryForUser(NSSystem.currentUserName());
    }
        
    protected boolean canCloseDocument() {
        if (isDocumentEdited()) {
            int result = NSAlertPanel.runAlert(NSBundle.localizedString("Close", "Title of alert panel which comes when the user tries to quit or close a window containing an unsaved document."), NSBundle.localizedString("Do you want to save changes to this document before closing?", "Question asked of user when he/she tries to close a window containing an unsaved document."), NSBundle.localizedString("Save", "Button choice which allows the user to save the document."), NSBundle.localizedString("Don't Save", "Button choice which allows the user to abort the save of a document which is being closed."), NSBundle.localizedString("Cancel", "Button choice allowing user to cancel."));

            if (result == NSAlertPanel.DefaultReturn) {
                if (!saveDocument(false, true)) {
                    return false;
                }
            } else if (result == NSAlertPanel.OtherReturn) {
                return false;
            }
        }

        return true;
    }

    // The showSavePanel argument causes the save panel to be shown.
    // rememberNewNameAndSuch causes the document's name, encoding, etc to be reset after the save
    // (basically this should be false for a "saveTo" operation and true otherwise).
    
    protected boolean saveDocument(boolean showSavePanel, boolean rememberNewNameAndSuch) {
        URL urlForSaving = documentURL();
        int encodingForSaving;
        boolean haveToChangeType = false;
        boolean showEncodingAccessory = false;

	if (isRichText()) {
            if (urlForSaving != null && NSPathUtilities.pathExtension(NSPathUtilities.pathFromURL(urlForSaving)).equals("rtfd")) {
                encodingForSaving = RichTextWithGraphicsStringEncoding;
            } else {
                encodingForSaving = textStorage.containsAttachments() ? RichTextWithGraphicsStringEncoding : RichTextStringEncoding;
                if ((encodingForSaving == RichTextWithGraphicsStringEncoding) && urlForSaving != null && NSPathUtilities.pathExtension(NSPathUtilities.pathFromURL(urlForSaving)).equals("rtf")) {
                    urlForSaving = null;		// Force the user to provide a name.
                }
                if (documentEncoding == Document.HTMLStringEncoding) {
                    urlForSaving = null; // Force the user to choose a new name
                }
            }
        } else {
            NSStringReference string = textStorage.stringReference();
            showEncodingAccessory = true;
            encodingForSaving = documentEncoding;
            if ((encodingForSaving != UnknownStringEncoding) && !string.canBeConvertedToEncoding(encodingForSaving)) {
                haveToChangeType = true;
                encodingForSaving = UnknownStringEncoding;
            }
            if (encodingForSaving == UnknownStringEncoding) {
                int defaultEncoding = Preferences.intValueForKey(Preferences.PlainTextEncodingForWrite);
                if (string.canBeConvertedToEncoding(defaultEncoding)) {
                    encodingForSaving = defaultEncoding;
                } else {
                    int[] plainTextEncoding = supportedEncodings();
                    for (int i = 0; i < plainTextEncoding.length; ++i) {
                        if (plainTextEncoding[i] >= 0 && plainTextEncoding[i] != defaultEncoding && plainTextEncoding[i] != NSStringReference.UnicodeStringEncoding && plainTextEncoding[i] != NSStringReference.UTF8StringEncoding && string.canBeConvertedToEncoding(plainTextEncoding[i])) {
                            encodingForSaving = plainTextEncoding[i];
                            break;
                        }
                    }
                }
                if (encodingForSaving == UnknownStringEncoding) {
                    encodingForSaving = NSStringReference.UnicodeStringEncoding;
                }
                if (haveToChangeType) {
                    Object[] localizedArgs = {
                        NSStringReference.localizedNameOfStringEncoding(documentEncoding),
                        NSStringReference.localizedNameOfStringEncoding(encodingForSaving)
                    };
                    
                    NSAlertPanel.runAlert(NSBundle.localizedString("Save Plain Text", "Title of save and alert panels when saving plain text"), MessageFormat.format(NSBundle.localizedString("Document can no longer be saved using its original {0} encoding. Please choose another encoding ({1} is one possibility).", "Contents of alert panel informing user that the file's string encoding needs to be changed"), localizedArgs), NSBundle.localizedString("OK", "OK"), null, null);
                }
            }
        }

        for (;;) {
            boolean goAhead = true;
            
            if (urlForSaving == null || haveToChangeType || showSavePanel) {
                boolean setEncoding = haveToChangeType || showEncodingAccessory;
                NSSavePanel panel = NSSavePanel.savePanel();

                switch (encodingForSaving) {
                    case RichTextStringEncoding:
                        panel.setRequiredFileType("rtf");
                        panel.setTitle(NSBundle.localizedString("Save RTF", "Title of save and alert panels when saving RTF"));
                        setEncoding = false;
                        break;

                    case RichTextWithGraphicsStringEncoding:
                        panel.setRequiredFileType("rtfd");
                        panel.setTitle(NSBundle.localizedString("Save RTFD", "Title of save and alert panels when saving RTFD"));
                        setEncoding = false;
                        break;

                    default:
                        panel.setTitle(NSBundle.localizedString("Save Plain Text", "Title of save and alert panels when saving plain text"));
                        if (setEncoding) {
                            panel.setAccessoryView(Document.encodingAccessory(encodingForSaving, false, false));
                            for (int cnt = 0; cnt < encodingPopUpButton.numberOfItems(); ++cnt) {
                                int encoding = encodingPopUpButton.itemAtIndex(cnt).tag();
                                if (encoding != UnknownStringEncoding && !textStorage.stringReference().canBeConvertedToEncoding(encoding)) {
                                    encodingPopUpButton.itemAtIndex(cnt).setEnabled(false);
                                }
                            }
                        }
                        break;
                }

                if (potentialSaveDirectory != null) {
                    Document.setLastOpenSavePanelDirectory(potentialSaveDirectory);
                }

                if ((((urlForSaving != null) && urlForSaving.getProtocol().equals("file")) ? panel.runModalInDirectory(NSPathUtilities.stringByDeletingLastPathComponent(NSPathUtilities.pathFromURL(urlForSaving)), NSPathUtilities.lastPathComponent(NSPathUtilities.pathFromURL(urlForSaving))) : panel.runModalInDirectory(Document.openSavePanelDirectory(), "")) != 0) {
                    urlForSaving = panel.URL();
                   
                    if (potentialSaveDirectory != null) {
                        setPotentialSaveDirectory(null);
                    }

                    if (setEncoding) {
                        encodingForSaving = encodingPopUpButton.selectedItem().tag();
                    }
                } else {
                    goAhead = false;
                }
            }

            if (goAhead) {
                if (TextReadWrite.saveToURL(urlForSaving, this, encodingForSaving, true)) {
                    if (rememberNewNameAndSuch) {
                        setEncoding(encodingForSaving);
                        setDocumentURL(urlForSaving);
                        setDocumentEdited(false);
                        NSDocumentController.sharedDocumentController().noteNewRecentDocumentURL(documentURL);
                        if (urlForSaving.getProtocol().equals("file")) {
                            Document.setLastOpenSavePanelDirectory(NSPathUtilities.stringByDeletingLastPathComponent(NSPathUtilities.pathFromURL(urlForSaving)));
                        }
                        undoManager().removeAllActions();
                    }
                    break;
                } else {
                    Object[] localizedArgs = {
                        NSPathUtilities.pathFromURL(urlForSaving)
                    };
                    
                    NSAlertPanel.runAlert(NSBundle.localizedString("Save Failed", "Title saying a document couldn't be saved."), MessageFormat.format(NSBundle.localizedString("Could not save document as {0}.", "Message indicating document couldn't be saved."), localizedArgs), NSBundle.localizedString("OK", "OK"), null, null);
                    urlForSaving = null;
                }
            } else {
                return false;
            }
        }

        return true;
    }
    
    public static Document documentForWindow(NSWindow window) {
        Object delegate = window.delegate();
        if (delegate != null && delegate instanceof Document) {
            return (Document) delegate;
        } else {
            return null;
        }
    }

    public static Document documentForURL(URL url) {
        NSArray windows = NSApplication.sharedApplication().windows();
        int cnt, numWindows = windows.count();

        url = cleanedUpURL(url);

        for (cnt = 0; cnt < numWindows; ++cnt) {
            Document document = Document.documentForWindow((NSWindow) windows.objectAtIndex(cnt));
            if (document != null) {
                URL docURL = document.documentURL();
                if (docURL != null && url.sameFile(docURL)) {
                    return document;
                }
            }
        }

        return null;
    }

    public static int numberOfOpenDocuments() {
        NSArray windows = NSApplication.sharedApplication().windows();
        int cnt, numWindows = windows.count(), numDocuments = 0;

        for (cnt = 0; cnt < numWindows; ++cnt) {
            if (Document.documentForWindow((NSWindow) windows.objectAtIndex(cnt)) != null) {
                ++numDocuments;
            }
        }

        return numDocuments;
    }

    // Menu validation.

    protected static void validateToggleItem(_NSObsoleteMenuItemProtocol aCell, boolean useFirst, String first, String second) {
        if (useFirst) {
            if (aCell.tag() != 42) {
                aCell.setTitleWithMnemonic(first);
                aCell.setTag(42);
            }
        } else {
            if (aCell.tag() != 43) {
                aCell.setTitleWithMnemonic(second);
                aCell.setTag(43);
            }
        }
    }

    public boolean validateMenuItem(_NSObsoleteMenuItemProtocol aCell) {
        String sel = aCell.action().name();

        if (sel.equals("toggleRich:")) {
            Document.validateToggleItem(aCell, isRichText(), NSBundle.localizedString("&Make Plain Text", "Menu item to make the current document plain text"), NSBundle.localizedString("&Make Rich Text", "Menu item to make the current document rich text"));
        } else if (sel.equals("togglePageBreaks:")) {
            Document.validateToggleItem(aCell, hasMultiplePages(), NSBundle.localizedString("&Wrap to Window", "Menu item to cause text to be laid out to size of the window"), NSBundle.localizedString("&Wrap to Page", "Menu item to cause text to be laid out to the size of the currently selected page type"));
        } else if (sel.equals("toggleHyphenation:")) {
            Document.validateToggleItem(aCell, hyphenationFactor() > 0f, NSBundle.localizedString("Disallow &Hyphenation", "Menu item to disallow hyphenation in the document"), NSBundle.localizedString("Allow &Hyphenation", "Menu item to allow hyphenation in the document"));
        } else if (sel.equals("revert:")) {
            return documentURL != null;
        }

        return true;
    }

    // Scripting support.

    public NSScriptObjectSpecifier objectSpecifier() {
        System.out.println("objectSpecifier");
        NSArray orderedDocs = (NSArray)NSKeyValue.valueForKey(NSApplication.sharedApplication(), "orderedDocuments");
        int index = orderedDocs.indexOfObject(this);
        // MF:!!! This should use NSNotFound when it is exposed to Java.
        if ((index >= 0) && (index < orderedDocs.count())) {
            NSScriptClassDescription desc = (NSScriptClassDescription)NSScriptClassDescription.classDescriptionForClass(NSApplication.class);
            return new NSIndexSpecifier(desc, null, "orderedDocuments", index);
        } else {
            return null;
        }
    }
    
    // We already have a textStorage() method implemented above.
    public void setTextStorage(Object ts) {
        // ts can actually be a string or an attributed string.
        if (ts instanceof NSAttributedString) {
            this.textStorage().replaceCharactersInRange(new NSRange(0, this.textStorage().length()), (NSAttributedString)ts);
        } else {
            this.textStorage().replaceCharactersInRange(new NSRange(0, this.textStorage().length()), (String)ts);
        }
    }

    public Object coerceValueForTextStorage(Object value) {
        // We want to just get Strings unchanged.  We will detect this and do the right thing in setTextStorage().  We do this because, this way, we will do more reasonable things about attributes when we are receiving plain text.
        if (value instanceof String) {
            return value;
        } else {
            return ((NSScriptCoercionHandler)NSScriptCoercionHandler.sharedCoercionHandler()).coerceValueToClass(value, NSTextStorage.class);
        }
    }

    // Since TextEdit's Document class does not (currently) subclass NSDocument, we must support all the NSDocument class keys and custom command handling by hand.  Fortunately this is pretty easy.
    
    // NSDocument's fileName key is the same as our documentName key.
    public String fileName() {
        if (this.documentURL().getProtocol().equals("file")) {
            return NSPathUtilities.pathFromURL(this.documentURL());
        } else {
            return null;
        }
    }
    
    public void setFileName(String name) {
        this.setDocumentURL(NSPathUtilities.URLWithPath(name));
    }

    public String lastComponentOfFileName() {
        if (this.documentURL().getProtocol().equals("file")) {
            return NSPathUtilities.lastPathComponent(NSPathUtilities.pathFromURL(this.documentURL()));
        } else {
            return null;
        }
    }
    
    public void setLastComponentOfFileName(String str) {
        if (this.documentURL().getProtocol().equals("file")) {
            String fileName = NSPathUtilities.pathFromURL(this.documentURL());
            String dirPath;

            if ((str == null) || str.equals("")) {
                // MF:??? Raise?
                return;
            }

            if ((fileName == null) || fileName.equals("")) {
                // MF:??? Is this a good default?  Could we get the default save panel location somehow?
                dirPath = NSSystem.currentHomeDirectory();
            } else {
                dirPath = NSPathUtilities.stringByDeletingLastPathComponent(fileName);
            }
            fileName = NSPathUtilities.stringByAppendingPathComponent(fileName, str);
            this.setDocumentURL(NSPathUtilities.URLWithPath(fileName));
        }
    }

    public Object handleSaveScriptCommand(NSScriptCommand command) {
        NSDictionary args = command.evaluatedArguments();
        String file = (String)args.objectForKey("File");

        if (file != null) {
            // We're being given a filename.  Do a Save As type operation using this new filename.
            // MF:??? Should this be Save To?
            this.setDocumentURL(NSPathUtilities.URLWithPath(file));
            this.save(null);
        } else {
            if (this.documentURL() != null) {
                // If the document has a filename, use it.
                this.save(null);
            } else {
                // MF:??? If the document has no file name, we should pay attention to the interaction level desired in the command.  If interaction is allowed we should run the save panel, if not, we should do nothing.
                // For now we just always run the save panel.
                this.saveAs(null);
            }
        }
        return null;
    }

    public Object handleCloseScriptCommand(NSCloseCommand command) {
        NSDictionary args = command.evaluatedArguments();
        String file = (String)args.objectForKey("File");
        int saveOptions = command.saveOptions();

        if (saveOptions == NSCloseCommand.SaveOptionsAsk) {
            if (file != null) {
                // If we were given a file name to save to, just do it.
                this.setDocumentURL(NSPathUtilities.URLWithPath(file));
                this.save(null);
                // Only close if we saved.
                if (!this.isDocumentEdited()) {
                    this.close(null);
                }
            } else {
                // If we're dirty, ask if we should save before closing.
                if (this.canCloseDocument()) {
                    this.close(null);
                }
            }
        } else if (saveOptions == NSCloseCommand.SaveOptionsYes) {
            // Save before closing.
            if (file != null) {
                this.setDocumentURL(NSPathUtilities.URLWithPath(file));
                this.save(null);
                this.close(null);
            } else {
                // Only save if we have a file name.  Only close if we saved.
                if (this.documentURL() != null) {
                    this.save(null);
                    if (!this.isDocumentEdited()) {
                        this.close(null);
                    }
                }
            }
        } else {
            // Don't save, just close.
            this.close(null);
        }
        return null;
    }

    public Object handlePrintScriptCommand(NSScriptCommand command) {
        // MF: This should eventually pay attention to the interaction level for scripting.  For now it always shows the panels.
        this.printDocumentUsingPrintPanel(true);
        return null;
    }

}

/*
 * An accessory class that is used to get ownership of the EncodingAccessory
 * nib file, and who advertises fields for Document to get the info its need.
 *
 */

class EncodingAccessoryOwner {
    public NSView encodingAccessory = null;
    public NSPopUpButton encodingPopUpButton = null;
    public NSButton ignoreRichTextButton = null;
    
    public boolean loadEncodingAccessory() {
        boolean success = NSApplication.loadNibNamed("EncodingAccessory", this);

        if (!success) {
            NSSystem.log("Couldn't load EncodingAccessory.nib");
        }
        
        return success;
    }
}

