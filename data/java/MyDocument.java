/*
	MyDocument.java
	Copyright (c) 2001-2004, Apple Computer, Inc., all rights reserved.
	Author: Chuck Pisula

	Milestones:
	Initially created 3/1/01

        Document and Toolbar Controller Object.
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

public class MyDocument extends NSDocument implements NSToolbarItem.ItemValidation {
    
    public  NSTextView 		documentTextView;	// The textview part of our document.
    public  NSTextField		searchFieldOutlet;	// "Template" textfield needed to create our toolbar searchfield item.
    private NSToolbarItem	activeSearchItem;	// A reference to the search field in the toolbar, null if the toolbar doesn't have one!
    private NSData		dataFromFile;		
    
    private static String 	MyDocToolbarIdentifier 		= "My Document Toolbar Identifier";
    private static String	SaveDocToolbarItemIdentifier 	= "Save Document Item Identifier";
    private static String	SearchDocToolbarItemIdentifier 	= "Search Document Item Identifier";
    
    // ==========================================================
    // Standard NSDocument methods
    // ==========================================================
    
    public MyDocument() {
        super();
    }
    
    public MyDocument(String fileName, String fileType) {
        super(fileName, fileType);
    }
    
    public String windowNibName() {
        return "MyDocument";
    }
        
    public void windowControllerDidLoadNib(NSWindowController  aController) {
        super.windowControllerDidLoadNib(aController);
	documentTextView.window().makeFirstResponder(documentTextView);
	documentTextView.window().setFrameAutosaveName("MyDocumentWindow");

	// Do the standard thing of loading in data we may have gotten if loadDataRepresentation() was used.
	if (dataFromFile!=null) {
	    this.loadTextViewWithInitialData(dataFromFile);
	    dataFromFile = null;
	}
	
	// Set up the toolbar after the document nib has been loaded.
	this.setupToolbar();
    }

    public NSData dataRepresentationOfType(String aType) {
	// Archive data in the format loadDocumentWithInitialData expects.
	NSData dataRepresentation = null;
	if (aType.equals("My Document Type")) {
	    dataRepresentation = documentTextView.RTFDFromRange(this.rangeOfEntireDocument());
	}
	return dataRepresentation;
    }

    public boolean loadDataRepresentation(NSData data, String aType) {
	boolean success = false;
	if (aType.equals("My Document Type")) {
	    if (documentTextView!=null) {
		this.loadTextViewWithInitialData(data);
	    } else {
		dataFromFile = data;
	    }
	    success = true;
	}
	return success;
    }
    
    private void loadTextViewWithInitialData(NSData data) {
	documentTextView.replaceCharactersInRangeWithRTFD(this.rangeOfEntireDocument(), data);
    }

    // ============================================================
    // NSToolbar Related Methods
    // ============================================================

    private void setupToolbar() {
	// Create a new toolbar instance, and attach it to our document window.
	NSToolbar toolbar = new NSToolbar(MyDocToolbarIdentifier);
	
	// Set up toolbar properties: Allow customization, give a default display mode, and remember state in user defaults.
	toolbar.setAllowsUserCustomization(true);
	toolbar.setAutosavesConfiguration(true);
	toolbar.setDisplayMode(NSToolbar.NSToolbarDisplayModeIconOnly);
	
	// We are the delegate
	toolbar.setDelegate(this);
	
	// Attach the toolbar to the document window.
	documentTextView.window().setToolbar(toolbar);
    }

    public NSToolbarItem toolbarItemForItemIdentifier(NSToolbar toolbar, String itemIdent, boolean willBeInserted)  {
	// Required delegate method.  Given an item identifier, this method returns an item.
	// The toolbar will use this method to obtain toolbar items that can be displayed in the customization sheet, or in the toolbar itself.
	NSToolbarItem toolbarItem = null;
	
	if (itemIdent.equals(SaveDocToolbarItemIdentifier)) {
            toolbarItem = new NSToolbarItem(itemIdent);
	    // Set the text label to be displayed in the toolbar and customization palette.
	    toolbarItem.setLabel("Save");
	    toolbarItem.setPaletteLabel("Save");
	    
	    // Set up a reasonable tooltip, and image.  Note, these aren't localized, but you will likely want to localize many of the item's properties.
	    toolbarItem.setToolTip("Save Your Document");
	    toolbarItem.setImage(NSImage.imageNamed("SaveDocumentItemImage"));
	    
	    // Tell the item what message to send when it is clicked.
	    toolbarItem.setTarget(this);
	    toolbarItem.setAction(new NSSelector("saveDocument", new Class[] { MyDocument.class }) );
	} else if(itemIdent.equals(SearchDocToolbarItemIdentifier)) {
            toolbarItem = new ValidatedViewToolbarItem(itemIdent);
            
	    // Set up the standard properties.
	    toolbarItem.setLabel("Search");
	    toolbarItem.setPaletteLabel("Search");
	    toolbarItem.setToolTip("Search Your Document");
	    
	    // Use a custom view, a text field, for the search item.
	    toolbarItem.setView(searchFieldOutlet);
	    toolbarItem.setMinSize(searchFieldOutlet.frame().size());
	    toolbarItem.setMaxSize(searchFieldOutlet.frame().size());

	    // By default, in text only mode, a custom items label will be shown as disabled text, but you can provide a 
	    // custom menu of your own by using <item>.setMenuFormRepresentation().
	    NSMenu submenu = new NSMenu();
	    NSMenuItem submenuItem = new NSMenuItem("Search Panel...", new NSSelector("searchUsingSearchPanel", new Class[] { MyDocument.class } ), "");
	    NSMenuItem menuFormRep = new NSMenuItem();

	    submenu.addItem(submenuItem);
	    submenuItem.setTarget(this);
	    menuFormRep.setSubmenu(submenu);
	    menuFormRep.setTitle(toolbarItem.label());

            // Normally, a menuFormRep with a submenu should just act like a pull down.  However, in 10.4 and later, the menuFormRep can have its own target / action.  If it does, on click and hold (or if the user clicks and drags down), the submenu will appear.  However, on just a click, the menuFormRep will fire its own action.
            menuFormRep.setTarget(this);
            menuFormRep.setAction(new NSSelector("searchMenuFormRepresentationClicked", new Class[] { MyDocument.class } ));
            
            // Please note, from a user experience perspective, you wouldn't set up your search field and menuFormRep like we do here.  This is simply an example which shows you all of the features you could use.
	    toolbarItem.setMenuFormRepresentation(menuFormRep);
	} else {
	    // itemIdent refered to a toolbar item that is not provide or supported by us or cocoa.
	    // Returning null will inform the toolbar this kind of item is not supported.
	    toolbarItem = null;
	}
	return toolbarItem;
    }
    
    public NSArray toolbarDefaultItemIdentifiers(NSToolbar toolbar) {
	// Required delegate method.  Returns the ordered list of items to be shown in the toolbar by default.   
	// If during the toolbar's initialization, no overriding values are found in the user defaults, or if the
	// user chooses to revert to the default items this set will be used.
	return new NSArray(new String[] { SaveDocToolbarItemIdentifier, NSToolbarItem.NSToolbarPrintItemIdentifier, NSToolbarItem.NSToolbarSeparatorItemIdentifier, 
					  NSToolbarItem.NSToolbarShowColorsItemIdentifier, NSToolbarItem.NSToolbarShowFontsItemIdentifier, NSToolbarItem.NSToolbarFlexibleItemIdentifier, 
					  NSToolbarItem.NSToolbarSpaceItemIdentifier, SearchDocToolbarItemIdentifier } );
    }
    
    public NSArray toolbarAllowedItemIdentifiers(NSToolbar toolbar) {
	// Required delegate method.  Returns the list of all allowed items by identifier.  By default, the toolbar 
	// does not assume any items are allowed, even the separator.  So, every allowed item must be explicitly listed.  
	// The set of allowed items is used to construct the customization palette.
	return new NSArray(new String[] { SearchDocToolbarItemIdentifier, SaveDocToolbarItemIdentifier, NSToolbarItem.NSToolbarPrintItemIdentifier, 
					  NSToolbarItem.NSToolbarShowColorsItemIdentifier, NSToolbarItem.NSToolbarShowFontsItemIdentifier, NSToolbarItem.NSToolbarCustomizeToolbarItemIdentifier,
					  NSToolbarItem.NSToolbarFlexibleItemIdentifier, NSToolbarItem.NSToolbarSpaceItemIdentifier, NSToolbarItem.NSToolbarSeparatorItemIdentifier } );
    }
  
    public void toolbarWillAddItem(NSNotification notif) {
        // Optional delegate method.  Before an new item is added to the toolbar, this notification is posted.  
	// This is the best place to notice a new item is going into the toolbar.  For instance, if you need to 
	// cache a reference to the toolbar item or need to set up some initial state, this is the best place 
	// to do it.   The notification object is the toolbar to which the item is being added.  The item being 
	// added is found by referencing the @"item" key in the userInfo.
	NSToolbarItem addedItem = (NSToolbarItem) notif.userInfo().objectForKey("item");
	if(addedItem.itemIdentifier().equals(SearchDocToolbarItemIdentifier)) {
	    activeSearchItem = addedItem;
	    activeSearchItem.setTarget(this);
	    activeSearchItem.setAction(new NSSelector("searchUsingToolbarSearchField", new Class[] { Object.class } ));
	} else if (addedItem.itemIdentifier().equals(NSToolbarItem.NSToolbarPrintItemIdentifier)) {
	    addedItem.setToolTip("Print Your Document");
	    addedItem.setTarget(this);
	}
    }  
    
    public void toolbarDidRemoveItem(NSNotification notif) {
	// Optional delegate method.  After an item is removed from a toolbar the notification is sent.  This allows 
	// the chance to tear down information related to the item that may have been cached.  The notification object
	// is the toolbar to which the item is being added.  The item being added is found by referencing the @"item"
	// key in the userInfo.
	NSToolbarItem removedItem = (NSToolbarItem) notif.userInfo().objectForKey("item");
	if (removedItem==activeSearchItem) {
	    activeSearchItem = null;    
	}
    }

    public boolean validateToolbarItem (NSToolbarItem toolbarItem) {
    	// Optional method.  This message is sent to us since we are the target of some toolbar item actions 
	// (for example:  of the save items action).
	boolean enable = false;
	if (toolbarItem.itemIdentifier().equals(SaveDocToolbarItemIdentifier)) {
	    // We will return true (ie. the button is enabled) only when the document is dirty and needs saving.
	    enable = this.isDocumentEdited();
	} else if (toolbarItem.itemIdentifier().equals(NSToolbarItem.NSToolbarPrintItemIdentifier)) {
	    enable = true;
	} else if (toolbarItem.itemIdentifier().equals(SearchDocToolbarItemIdentifier)) {
            enable = documentTextView.textStorage().toString().length()>0;
        }
	return enable;
    }
    
    public boolean validateMenuItem(NSMenuItem item) {
        boolean enabled = true;
        String actionAsString = item.action().toString();
        
        if (actionAsString=="searchMenuFormRepresentationClicked" || actionAsString=="searchUsingSearchPanel") {
            enabled = this.validateToolbarItem(activeSearchItem);
        }

        return enabled;
    }

    // ============================================================
    // Utility Methods : Misc, and Target/Actions Methods
    // ============================================================

    private NSRange rangeOfEntireDocument() {
	// Convenience method: Compute and return the range that encompasses the entire document.
	int length = 0;
	if (documentTextView.string()!=null) {
	    length = documentTextView.string().length();
	}
	return new NSRange(0,length);
    }

    public void printDocument(Object sender) {
	// This message is send by the print toolbar item.
	NSPrintOperation printOperation = NSPrintOperation.printOperationWithView(documentTextView, new NSPrintInfo());
	printOperation.runModalOperation(documentTextView.window(), null, null, null);
    }
    
    public NSArray rangesOfStringInDocument(String searchString) {
        NSStringReference string = documentTextView.textStorage().stringReference();
        NSMutableArray ranges = new NSMutableArray();
        
        NSRange thisCharRange, searchCharRange;
        searchCharRange = new NSRange(0, string.length());
        while (searchCharRange.length()>0) {
            thisCharRange = string.rangeOfString(searchString, 0, searchCharRange);
            if (thisCharRange.length()>0) {
                searchCharRange = new NSRange(thisCharRange.location() + thisCharRange.length(), string.length() - (thisCharRange.location() + thisCharRange.length()));
                ranges.addObject(new NSDictionary(thisCharRange, "rangeValue"));
            } else {
                searchCharRange = new NSRange(NSArray.NotFound, 0);
            }
        }
        return ranges;
    }

    public void searchUsingToolbarSearchField(Object sender) {
        // This message is sent when the user strikes return in the search field in the toolbar 
    	String searchString = ((NSTextField)activeSearchItem.view()).stringValue();
        NSArray rangesOfString = this.rangesOfStringInDocument(searchString);
        if (rangesOfString.count()>0) {
            NSSelector multipleRangeSelector = new NSSelector("setSelectedRanges", new Class[] { NSArray.class });
            if (multipleRangeSelector.implementedByObject(documentTextView)) {
                // NSTextView can handle multiple selections in 10.4 and later.
                // documentTextView.setSelectedRangs(rangesOfString);
            } else {
                // If we can't do multiple selection, just select the first range.
                documentTextView.setSelectedRange( (NSRange) ((NSDictionary)rangesOfString.objectAtIndex(0)).objectForKey("rangeValue") );
            }
        }
    }

    public void searchMenuFormRepresentationClicked(Object sender) {
        documentTextView.window().toolbar().setDisplayMode(NSToolbar.DisplayModeIconOnly);
        documentTextView.window().makeFirstResponder(activeSearchItem.view());
    }

    public void searchUsingSearchPanel(Object sender) {
	// This message is sent from the search items custom menu representation.
    	String searchString = ((NSTextField)activeSearchItem.view()).stringValue();
	NSAlertPanel.beginInformationalAlertSheet ( "searchUsingSearchPanel is not implemented (left as an exercise to the reader...)","","","",documentTextView.window(),null,null,null,null,"");
    }
}
