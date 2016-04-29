//
//  NSMovieView.java
//  NSMovieSample
//
//  Created by Steve Lewallen on Mon Jan 13 2003.
//  Copyright (c) 2003 Apple Computer. All rights reserved.
//

import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import com.apple.eawt.CocoaComponent;

class MediaItem {
    String url, title;
    
    public MediaItem(String url, String title) {
        this.url = url;
        this.title = title;
    }
    
    public String toString() {
        return title;
    }
}

class MediaChoice extends Choice {
    Hashtable choices = new Hashtable();
    
    public void add(MediaItem item) {
        choices.put(item.title, item.url);
        add(item.title);
    }
    
    public String getChoice(Object key) {
        return (String)choices.get(key);
    }
}

/**
 * This is a sample to demonstrate how to embed custom NSViews into the AWT component hierarchy.  Embedded views
 * will receive all of the normal NS events and act as they normally do in an NSView view hierarchy.  The 
 * corresponding Java Component will receive all expected AWT events.  See the documentation for the
 * com.apple.eawt.CocoaComponent for more details.
 *<p>
 * Please Note : This sample uses the NSMovieView.  The NSMovieView will not paint all of its content area when
 * not displaying images or video.  Instead, garbage will remain on screen in this unpainted area.  This is a
 * behavior of NSMovieView and not of this sample, nor AWT.
 *
 */
public class NSMovieView extends CocoaComponent {

    public static final int kSetMovieUrl 	= 0;
    public static final int kPlayMovie 		= 1;
    public static final int kStopMovie 		= 2;
    
    static {
        try {
            System.loadLibrary("sample");
        }
        catch(Throwable t) {
            t.printStackTrace();
        }
    }
    
    public native long createNSViewLong();
	
	// createNSView() is deprecated in Tiger and later,
	// but because it is an abstract method in CocoaComponent,
	// we must provide an implementation.
	// Because this example returns 0, if you want to be 
	// compatible with older systems, you'll need to provide
	// a real implementation here.
	public int createNSView() {
		return 0;
	}
	    
    public static void main(String args[]) {
        Frame f = new Frame("Sample AWT Media Application");
        f.setLayout(new FlowLayout(FlowLayout.CENTER));
        f.setBounds(100, 100, 720, 320);
        
        // Create and add the custom NSMovieView to the AWT Frame
        final NSMovieView movieView = new NSMovieView();
        f.add(movieView);
        
        // Create and add the movie's controls
        Panel controlPanel = new Panel();
        controlPanel.setLayout(new BorderLayout());
        f.add(controlPanel);
        
        //
        // Media files Jaguar and dev tools include :
        //
        final MediaChoice mediaChoice = new MediaChoice();
        mediaChoice.add(new MediaItem("","  --- choose media, then press play ---  "));
        mediaChoice.add(new MediaItem("file:///Developer/Examples/Java/AppleDemos/CocoaComponent/NSMovieSample/Media/jumps.mov", "QuickTime Sample Movie \"Jumps\""));
        mediaChoice.add(new MediaItem("file:///Developer/Examples/Java/AppleDemos/CocoaComponent/NSMovieSample/Media/sound.aif", "QuickTime Sample AIF \"sound.aif\""));
        mediaChoice.add(new MediaItem("file:///Developer/Examples/Java/AppleDemos/CocoaComponent/NSMovieSample/Media/TextOnly.mov", "QuickTime Sample Text Only"));
        // mediaChoice.add(new MediaItem("http://www.apple.com/quicktime/overview/media/wessinit.mp3", "QuickTime MP3 Demo"));
        // mediaChoice.add(new MediaItem("http://www.apple.com/quicktime/overview/media/PlanetRise.mov", "QuickTime Sprite Water Demo"));
        // mediaChoice.add(new MediaItem("http://www.apple.com/quicktime/overview/media/playme.mov", "QuickTime Sprite Game Demo"));
        // mediaChoice.add(new MediaItem("http://www.apple.com/quicktime/overview/media/qtflashdemo.mov", "QuickTime Flash Demo"));
        // mediaChoice.add(new MediaItem("http://stream.qtv.apple.com/events/may/preview/ministerofpain_http_40.mov", "Minister Of Pain Stream"));
        mediaChoice.add(new MediaItem("http://www.theonering.net/perl/redir.pl?f=torn4qt_med.mov", "Lord of the Rings news from TORN Digital"));
        mediaChoice.addItemListener(new ItemListener()  {

            public void itemStateChanged(ItemEvent e) {
                movieView.setMovieUrl(mediaChoice.getChoice(e.getItem()));
            }

        });
        controlPanel.add(mediaChoice, BorderLayout.NORTH);
        
        Panel subPanel = new Panel();
        controlPanel.add(subPanel, BorderLayout.SOUTH);
        
        Button playButton = new Button("Play");
        playButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                movieView.play();
            }        
        
        });
        subPanel.add(playButton);
        
        Button stopButton = new Button("Stop");
        stopButton.addActionListener(new ActionListener()  {
            public void actionPerformed(ActionEvent e) {
                movieView.stop();
            }        
        });
        subPanel.add(stopButton);

        // Add Event Samplers
        // (This is simply to demonstrate CocoaComponents receive AWT events)
        addKeyListener(f);
        addMouseMotionListener(f);
        addFocusListener(f);
        addMouseWheelListener(f);
        addMouseListener(f);
        
        f.show();
        
    }
    
    /*** Media API ***/
    
    public void setMovieUrl(String url) {
        sendMessage(kSetMovieUrl, url);
    }
    
    public void play() {
        sendMessage(kPlayMovie, null);
    }
    
    public void stop() {
        sendMessage(kStopMovie, null);
    }
    
    /*** Dimension API Used With AWT Layout Managers ***/
    
    public Dimension getMaximumSize() {
        return new Dimension(Short.MAX_VALUE, Short.MAX_VALUE);
    }
    
    public Dimension getMinimumSize() {
        return new Dimension(160, 120);
    }
    
    public Dimension getPreferredSize() {
        return new Dimension(320, 240);
    }


    /********** These are utility API to demonstrate that we receive events on CocoaComponents *************/
    public static void addKeyListener(Container c) {
        Component[] components = c.getComponents();
        KeyListener listener = new KeyListener() {
            public void keyTyped(KeyEvent e) {
                System.err.println("KeyListener Event : " + e);
            }
            public void keyPressed(KeyEvent e) {
                System.err.println("KeyListener Event : " + e);
            }
            public void keyReleased(KeyEvent e) {
                System.err.println("KeyListener Event : " + e);
            }
        };
        c.addKeyListener(listener);
        
        for (int i = 0; i < components.length; i++) {
            if (components[i] instanceof Container)
                addKeyListener((Container)components[i]);
             else components[i].addKeyListener(listener);
       }
    }
    
    public static void addMouseMotionListener(Container c) {
        Component[] components = c.getComponents();
        MouseMotionListener listener = new MouseMotionListener() {
            public void mouseDragged(MouseEvent e) {
                System.err.println("MouseMotionListener Event : " + e);
            }
            public void mouseMoved(MouseEvent e) {
                System.err.println("MouseMotionListener Event : " + e);
            }
        };
        c.addMouseMotionListener(listener);
        
        for (int i = 0; i < components.length; i++) {
            if (components[i] instanceof Container)
                addMouseMotionListener((Container)components[i]);
            else components[i].addMouseMotionListener(listener);
        }
    }
    
    public static void addFocusListener(Container c) {
        Component[] components = c.getComponents();
        FocusListener listener = new FocusListener() {
            public void focusGained(FocusEvent e) {
                System.err.println("FocusListener Event : " + e);
            }
            public void focusLost(FocusEvent e) {
                System.err.println("FocusListener Event : " + e);
            }
        };
        c.addFocusListener(listener);
        
        for (int i = 0; i < components.length; i++) {
            if (components[i] instanceof Container)
                addFocusListener((Container)components[i]);
            else components[i].addFocusListener(listener);
        }
    }
    
    
    public static void addMouseWheelListener(Container c) {
            
        Component[] components = c.getComponents();
        MouseWheelListener listener = new MouseWheelListener() {
            public void mouseWheelMoved(MouseWheelEvent e) {
                System.err.println("MouseWheelListener Event : " + e);
            }
        };
        c.addMouseWheelListener(listener);
        
        for (int i = 0; i < components.length; i++) {
            if (components[i] instanceof Container)
                addMouseWheelListener((Container)components[i]);
            else components[i].addMouseWheelListener(listener);
        }

    }
    
    public static void addMouseListener(Container c) {
        Component[] components = c.getComponents();
        MouseListener listener = new MouseListener() {
            public void mouseClicked(MouseEvent e) {
                System.err.println("MouseListener Event : " + e);
            }
            public void mousePressed(MouseEvent e) {
                System.err.println("MouseListener Event : " + e);
            }
            public void mouseReleased(MouseEvent e) {
                System.err.println("MouseListener Event : " + e);
            }
            public void mouseEntered(MouseEvent e) {
                System.err.println("MouseListener Event : " + e);
            }
            public void mouseExited(MouseEvent e) {
                System.err.println("MouseListener Event : " + e);
            }
        };
        c.addMouseListener(listener);
        
        for (int i = 0; i < components.length; i++) {
            if (components[i] instanceof Container)
                addMouseListener((Container)components[i]);
            else  components[i].addMouseListener(listener);

        }
    }


}
