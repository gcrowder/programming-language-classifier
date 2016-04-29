//
//  «FILENAME»
//  «PROJECTNAME»
//
//  Created by «FULLUSERNAME» on «DATE».
//  Copyright «YEAR» «ORGANIZATIONNAME». All rights reserved.
//

import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;


public class «FILEBASENAMEASIDENTIFIER» extends NSDocument {

    public NSString windowNibName() {
        // Implement this to return a nib to load OR implement -makeWindowControllers to manually create your controllers.
        return "«FILEBASENAMEASIDENTIFIER»";
    }
    
    public NSData dataRepresentationOfType(String type) {
        // Implement to provide a persistent data representation of your document OR remove this and implement the file-wrapper or file path based save methods.
        return null;
    }
    
    public boolean loadDataRepresentation (NSData data, String type) {
        // Implement to load a persistent data representation of your document OR remove this and implement the file-wrapper or file path based load methods.
        return true;
    }

}
