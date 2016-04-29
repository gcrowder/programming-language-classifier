
import com.apple.cocoa.foundation.*;
import com.apple.cocoa.application.*;
import java.lang.*;


public class ValidatedViewToolbarItem extends NSToolbarItem {

    public ValidatedViewToolbarItem(String identifier) {
        super(identifier);
    }

    public static NSSelector actionSelectorFromSelector(NSSelector selector) {
        // Create a NSSelector that contains the method and the correct argument list.
        // Note: Due to a bug in NSSelector, objects sometime return NSSelectors with ":"s in the method names.  Comparisons fail when there are ObjC ":"s in the method name.  So, this method checks for trailing colons as a workaround and strips them off.
        String selectorString = selector.name();
        Class[] parameterTypes = new Class[] { Object.class };
        
        if (selectorString.indexOf(':') == selectorString.length() - 1) {
            selectorString = selectorString.substring(0, selectorString.length() - 1);
        }
        
        return new NSSelector(selectorString, parameterTypes);
    }

    public void validate() {
        super.validate(); // Let super take care of validating the menuFormRep, etc.

        try {
            NSControl control = (NSControl)this.view();
            Object target = control.target();
            
            if (actionSelectorFromSelector(control.action()).implementedByObject(target)) {
                NSSelector validationSelector = new NSSelector("validateToolbarItem", new Class[] { NSToolbarItem.class } );
                boolean enable = true;
                
                if (validationSelector.implementedByObject(target)) {
                    enable = ((NSToolbarItem.ItemValidation)target).validateToolbarItem(this);
                }
                this.setEnabled(enable);
                control.setEnabled(enable);
            }
        } catch (Exception e) {
            // Catch any exceptions.  We assume that the view is an NSControl, etc...
            System.out.println("Exception " + e + "\n");
        }
    }
}
