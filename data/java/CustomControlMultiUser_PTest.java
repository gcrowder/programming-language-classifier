/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 * 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTView;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomControl;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomDomainModelReference;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomFactory;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Eugen Neufeld
 * 
 */
@RunWith(DatabindingClassRunner.class)
public class CustomControlMultiUser_PTest {

	private Fan fan1;
	private Fan fan2;
	private VView view1;
	private VView view2;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		fan1 = BowlingFactory.eINSTANCE.createFan();
		{
			final Merchandise m1 = BowlingFactory.eINSTANCE.createMerchandise();
			m1.setName("1");
			fan1.setFavouriteMerchandise(m1);
		}

		fan2 = BowlingFactory.eINSTANCE.createFan();
		{
			final Merchandise m2 = BowlingFactory.eINSTANCE.createMerchandise();
			m2.setName("2");
			fan2.setFavouriteMerchandise(m2);
		}

		final VCustomControl control1 = VCustomFactory.eINSTANCE.createCustomControl();
		final VCustomControl control2 = VCustomFactory.eINSTANCE.createCustomControl();

		final VCustomDomainModelReference hard1 = VCustomFactory.eINSTANCE.createCustomDomainModelReference();
		final VCustomDomainModelReference hard2 = VCustomFactory.eINSTANCE.createCustomDomainModelReference();

		hard1.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		hard2.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		hard1.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub2");
		hard2.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub2");

		control1.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		control2.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		control1.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub2");
		control2.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub2");

		control1.setDomainModelReference(hard1);
		control2.setDomainModelReference(hard2);

		view1 = VViewFactory.eINSTANCE.createView();
		view1.getChildren().add(control1);

		view2 = VViewFactory.eINSTANCE.createView();
		view2.getChildren().add(control2);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() throws ECPRendererException {
		final Shell shell1 = new Shell();
		final Shell shell2 = new Shell();
		shell1.open();
		shell2.open();

		final ECPSWTView swtView1 = ECPSWTViewRenderer.INSTANCE.render(shell1, fan1, view1);

		final ECPSWTView swtView2 = ECPSWTViewRenderer.INSTANCE.render(shell2, fan2, view2);

		final Composite comp1 = (Composite) swtView1.getSWTControl();
		final Composite comp2 = (Composite) swtView2.getSWTControl();

		final String text1 = ((Text) ((Composite) ((Composite) ((Composite) comp1.getChildren()[1])
			.getChildren()[0]).getChildren()[0]).getChildren()[0]).getText();
		final String text2 = ((Text) ((Composite) ((Composite) ((Composite) comp2.getChildren()[1])
			.getChildren()[0]).getChildren()[0]).getChildren()[0]).getText();

		Assert.assertNotEquals(text1, text2);

		shell1.close();
		shell1.dispose();

		shell2.close();
		shell2.dispose();
	}
}
