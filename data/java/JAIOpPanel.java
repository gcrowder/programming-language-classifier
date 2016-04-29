/**
 * @(#)JAIOpPanel.java	15.2 03/05/20
 *
 * Copyright (c) 2003 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * -Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * -Redistribution in binary form must reproduct the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of Sun Microsystems, Inc. or the names of contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
 * ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS
 * SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF
 * USING, MODIFYING OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO
 * EVENT WILL SUN OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT
 * OR DATA, OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR
 * PUNITIVE DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF
 * LIABILITY, ARISING OUT OF THE USE OF OR INABILITY TO USE SOFTWARE, EVEN
 * IF SUN HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * You acknowledge that Software is not designed,licensed or intended for
 * use in the design, construction, operation or maintenance of any nuclear
 * facility.
 */



import java.awt.*;
import java.awt.event.*;
import java.awt.image.renderable.ParameterBlock;
import java.util.Hashtable;
import java.util.Vector;
import javax.media.jai.*;
import javax.swing.*;
import javax.swing.event.*;
import java.awt.image.DataBuffer;
import java.awt.image.SampleModel;

public class JAIOpPanel extends JAIDemoPanel {
  
  String opName;
  int id;
  Icon imageIcon = null;
  JLabel imageLabel = null;
  Vector sourceVec;
  JAIDyadicPanel demo;

  public JAIOpPanel(JAIDyadicPanel demo, int id, String opName,Vector sourceVec ){
    super(sourceVec);
    this.opName = opName;
    this.sourceVec = sourceVec;
    masterSetup();
    this.id = id;
    this.demo = demo;
  }  
   
  public String getDemoName(){
    return opName;
  }
  public void makeControls(JPanel controls){ 
    controls.setLayout(new BorderLayout());
  }
 
 public PlanarImage process() {
   PlanarImage im,dst1;
   double[] constants;
//   SampleModel sm;
   int nBands;
   Rectangle rect;
  // ImageLayout il;
//   RenderingHints rh;
   
   PlanarImage im0 = getSource(0);
   PlanarImage im1 = getSource(1);
   
   ParameterBlock pb = new ParameterBlock();
   pb.addSource(im1);
   pb.addSource(im0); 

   
   switch(id){
   case 0: return im0;
   case 1: return im1;
     
   case 2:
     return JAI.create("add", pb, renderHints);
   case 3:
     return JAI.create("subtract", pb, renderHints);
   case 4:
     rect = im0.getBounds().intersection(im1.getBounds());
     nBands = Math.min(im0.getSampleModel().getNumBands(),
			   im1.getSampleModel().getNumBands());
     
     pb = new ParameterBlock();
     pb.addSource(im0);
     pb.addSource(im1);
     im = JAI.create("multiply",pb,getRenderingHints(DataBuffer.TYPE_USHORT,
						     rect,nBands));

     // Constants
     constants = new double[3];
     constants[0] = 255.0;
     constants[1] = 255.0;
     constants[2] = 255.0;
 
     pb = new ParameterBlock();
     pb.addSource(im);
     pb.add(constants);
     dst1 = (PlanarImage)JAI.create("dividebyconst", pb,
				    getRenderingHints(DataBuffer.TYPE_BYTE,
						      rect,nBands));     
     return dst1;
     
     
   case 5:
     
     rect = im0.getBounds().intersection(im1.getBounds());
     nBands = Math.min(im0.getSampleModel().getNumBands(),
			   im1.getSampleModel().getNumBands());
    
     
     pb = new ParameterBlock();
     pb.addSource(im0);
     pb.addSource(im1);
     im = JAI.create("divide",pb,getRenderingHints(DataBuffer.TYPE_FLOAT,
						     rect,nBands));


     // Constants
     constants = new double[3];
     constants[0] = 255.0;
     constants[1] = 255.0;
     constants[2] = 255.0;
  
     pb = new ParameterBlock();
     pb.addSource(im);
     pb.add(constants);
     dst1 = (PlanarImage)JAI.create("multiplyconst", pb,
				    getRenderingHints(DataBuffer.TYPE_BYTE,
						      rect,nBands));
     return dst1;
   
   default:
     return im0;
   }
    
    }
  
  RenderingHints getRenderingHints(int dType,Rectangle rect,int nBands){
    SampleModel sm =
      RasterFactory.createPixelInterleavedSampleModel(dType,
						      rect.width,
						      rect.height,
						      nBands);
    
    ImageLayout il = new ImageLayout();
    il.setSampleModel(sm);
    return new RenderingHints(JAI.KEY_IMAGE_LAYOUT, il);
  }

  public void reset(){
   demo.reset();
  }

}
