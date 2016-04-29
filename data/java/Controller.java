/*
        Controller.java
        Copyright (c) 1995-2005 by Apple Computer, Inc., all rights reserved.
        Author: Yves Arrouye
        Java translation of original code by Ali Ozer

        Central controller object for TextEdit...
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
import java.text.MessageFormat;
import java.lang.reflect.*;

public class Controller {

    public void applicationDidFinishLaunching (NSNotification notification) {
        // To get service requests to go to the controller...
        NSApplication.sharedApplication().setServicesProvider(this);
    }

    public boolean applicationShouldTerminate (NSApplication app) {
        NSArray windows = app.windows();
        int count = windows.count();
        boolean needsSaving = false;

        // Determine if there are any unsaved documents

        while (!needsSaving && (0 != count--)) {
            NSWindow window = (NSWindow) windows.objectAtIndex(count);
            Document document = Document.documentForWindow(window);
            if (null != document) {
                if (document.isDocumentEdited())
                    needsSaving = true;
            }
        }

        if (needsSaving) {
            int choice = NSAlertPanel.runAlert(
		NSBundle.localizedString("Quit", "Title of alert panel which comes up when user chooses Quit and there are unsaved documents."),
		NSBundle.localizedString("You have unsaved documents.", "Message in the alert panel which comes up when user chooses Quit and there are unsaved documents."),
		NSBundle.localizedString("Review Unsaved", "Choice (on a button) given to user which allows him/her to review all unsaved documents if he/she quits the application without saving them all first."),
		NSBundle.localizedString("Quit Anyway", "Choice (on a button) given to user which allows him/her to quit the application even though there are unsaved documents."),
		NSBundle.localizedString("Cancel", "Cancel"));

            if (choice == NSAlertPanel.OtherReturn) {
                // Cancel
                return false;
            } else if (choice != NSAlertPanel.AlternateReturn) {
                // Review unsaved; Quit Anyway falls through
                count = windows.count();
                while (0 != count--) {
                    NSWindow window = (NSWindow) windows.objectAtIndex(count);
                    Document document = Document.documentForWindow(window);
                    if (null != document) {
                        window.makeKeyAndOrderFront(null);
                        if (false == document.canCloseDocument())
                            return false;
                    }
                }
            }
        }

        Preferences.saveDefaults();
        return true;
    }

    public boolean applicationOpenFile (NSApplication app, String filename) {
        // If the document is in a .rtfd and its name is TXT.rtf or index.rtf, open the
        // parent dir... This allows opening RTFDs on Windows, where you can't "open" folders.
        String parentDir = NSPathUtilities.stringByDeletingLastPathComponent(filename);
        String extension = NSPathUtilities.pathExtension(parentDir);
        if (extension.toLowerCase().equals("rtfd")) {
            String lastPathComponent = NSPathUtilities.lastPathComponent(filename);
            lastPathComponent = lastPathComponent.toLowerCase();
            if (lastPathComponent.equals("txt.rtf") || lastPathComponent.equals("index.rtf")) {
                filename = parentDir;
            }
        }
        return Document.openDocumentWithURL(NSPathUtilities.URLWithPath(filename), Document.UnknownStringEncoding, Preferences.booleanValueForKey(Preferences.IgnoreRichText), Preferences.booleanValueForKey(Preferences.IgnoreHTML)) != null;
    }

    public boolean applicationOpenTempFile (NSApplication app, String filename) {
        return this.applicationOpenFile(app, filename);
    }

    public boolean applicationOpenUntitledFile (NSApplication app) {
        return Document.openUntitled() != null;
    }

    public boolean applicationPrintFile(NSApplication app, String filename) {
        // If the document is in a .rtfd and its name is TXT.rtf or index.rtf, open the
        // parent dir... This allows opening RTFDs on Windows, where you can't "open" folders.
        String parentDir = NSPathUtilities.stringByDeletingLastPathComponent(filename);
        String extension = NSPathUtilities.pathExtension(parentDir);
        if (extension.toLowerCase().equals("rtfd")) {
            String lastPathComponent = NSPathUtilities.lastPathComponent(filename);
            lastPathComponent = lastPathComponent.toLowerCase();
            if (lastPathComponent.equals("txt.rtf") || lastPathComponent.equals("index.rtf")) {
                filename = parentDir;
            }
        }
        boolean result = false;
        Document doc = Document.openDocumentWithURL(NSPathUtilities.URLWithPath(filename), Document.UnknownStringEncoding, Preferences.booleanValueForKey(Preferences.IgnoreRichText), Preferences.booleanValueForKey(Preferences.IgnoreHTML));
        if (doc != null) {
            // Avoid the print panel unless we don't have a default printer chosen.
            boolean useUI = ((NSPrintInfo.defaultPrinter() != null) ? false : true);
            doc.printDocumentUsingPrintPanel(useUI);
            doc.close(this);
            result = true;
        }
        
        return result;
    }

    public void createNew(Object sender) {
        Document.openUntitled();
    }

    public void open(Object sender) {
        Document.open(sender);
    }

    public void saveAll(Object sender) {
        NSArray windows = NSApplication.sharedApplication().windows();
        int count = windows.count();

        while (0 != count--) {
            NSWindow window = (NSWindow) windows.objectAtIndex(count);
            Document document = Document.documentForWindow(window);
            if (null != document) {
                if (document.isDocumentEdited()) {
                    if (false == document.saveDocument(false, true))
                        return;
                }
            }
        }
    }

    // Services support

    String servicesOpenFile(NSPasteboard pboard, String data) {
        boolean success = false;
        NSArray types = pboard.types();
        String filename = "";

        if (types.containsObject(NSPasteboard.StringPboardType)){
            filename = pboard.stringForType(NSPasteboard.StringPboardType);
            if (filename != null) {
                filename = NSPathUtilities.stringByExpandingTildeInPath(filename);	// Convert the "~username" case
                if (filename != null) success = (Document.openDocumentWithURL(NSPathUtilities.URLWithPath(filename), Document.UnknownStringEncoding, Preferences.booleanValueForKey(Preferences.IgnoreRichText), Preferences.booleanValueForKey(Preferences.IgnoreHTML)) != null);
            }
        }

        if (!success) {
            Object[] localizedArgs = {filename};
	    NSAlertPanel.runAlert(NSBundle.localizedString("Open File Failed", "Title of alert indicating error during Open File service"), MessageFormat.format(NSBundle.localizedString("Could not open file {0}.", "Message indicating file couldn't be opened; {0} is the filename."), localizedArgs), NSBundle.localizedString("OK", "OK"), null, null);
        }
	return null;	// No need to report an error string...
    }

    String servicesOpenSelection(NSPasteboard pboard, String data) {
        boolean success = false;
        NSArray types = pboard.types();
        String filename = "";
        Document document = Document.openUntitled();
        String preferredType = document.firstTextView().preferredPasteboardTypeFromArray(types, null);

        if (preferredType != null) {
            document.setRichText(preferredType != NSPasteboard.StringPboardType);	// Special case to open a plain text document
            success = document.firstTextView().readSelectionFromPasteboardOfType(pboard, preferredType);
            document.setDocumentURL(null);
        }

        if (!success) {
	    NSAlertPanel.runAlert(NSBundle.localizedString("Open Selection Failed", "Title of alert indicating error during Open Selection service"), NSBundle.localizedString("Could not open selection.", "Message indicating selection couldn't be opened during Open Selection service"), NSBundle.localizedString("OK", "OK"), null, null);
        }
        return null;	// No need to report an error string...
    }
    
    // Scripting support.
    
    public NSArray orderedDocuments() {
        NSApplication app = NSApplication.sharedApplication();
        NSArray orderedWindows = (NSArray)NSKeyValue.valueForKey(app, "orderedWindows");
        int i, c = orderedWindows.count();
        NSMutableArray orderedDocs = new NSMutableArray();
        Object curDelegate;

        for (i=0; i<c; i++) {
            curDelegate = ((NSWindow)orderedWindows.objectAtIndex(i)).delegate();
            if ((curDelegate != null) && (curDelegate instanceof Document)) {
                orderedDocs.addObject(curDelegate);
            }
        }
        return orderedDocs;
    }

    public boolean applicationDelegateHandlesKey(NSApplication application, String key) {
        if (key.equals("orderedDocuments")) {
            return true;
        } else {
            return false;
        }
    }

    public void insertInOrderedDocumentsAtIndex(Document doc, int index) {
        doc.firstTextView().setSelectedRange(new NSRange(0, 0));
        doc.setDocumentURL(null);
        doc.setDocumentEdited(false);
        doc.setPotentialSaveDirectory(Document.openSavePanelDirectory());
        doc.window().makeKeyAndOrderFront(null);
    }

    public boolean validateMenuItem(_NSObsoleteMenuItemProtocol aCell) {
        String sel = aCell.action().name();

        if (sel.equals("saveAll:")) {	    // If there are any documents at all, we enable the saveAll: menu item
            NSArray windows = NSApplication.sharedApplication().windows();
            int count = windows.count();
            while (0 != count--) {
                NSWindow window = (NSWindow)windows.objectAtIndex(count);
                if (Document.documentForWindow(window) != null) return true;
            } 
	    return false;           
        }
        return true;
   }
}
