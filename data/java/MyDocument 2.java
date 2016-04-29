//
//  MyDocument.java
//  ÇPROJECTNAMEÈ
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ . All rights reserved.
//

import com.apple.cocoa.application.*;
import com.apple.cocoa.foundation.*;

public class MyDocument extends NSDocument {

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
        // Add any code here that needs to be executed once the windowController has loaded the document's window.
    }

    public NSData dataRepresentationOfType(String aType) {
        // Insert code here to create and return the data for your document.
		// For applications targeted for Tiger or later systems, you should use the new Tiger API dataOfType.  In this case you can also choose to override writeToURLOfType, or fileWrapperOfType instead.
        return null;
    }

    public boolean loadDataRepresentation(NSData data, String aType) {
        // Insert code here to read your document from the given data.
		// For applications targeted for Tiger or later systems, you should use the new Tiger API readFromDataOfType.  In this case you can also choose to override readFromURLOfType or readFromFileWrapperOfType instead.
        return true;
    }
}
