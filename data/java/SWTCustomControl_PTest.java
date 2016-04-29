/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.ecp.view.spi.custom.model.VCustomControl;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomDomainModelReference;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(DatabindingClassRunner.class)
public class SWTCustomControl_PTest {

	/**
	 * @author Jonas
	 *
	 */
	public class TestHandel {

		private final VView view;

		private final VControl customControl;

		/**
		 * @param view
		 * @param customControl
		 */

		public TestHandel(VView view, VControl customControl) {
			this.view = view;
			this.customControl = customControl;
		}

		/**
		 * @return the view
		 */
		public VView getView() {
			return view;
		}

		/**
		 * @return the customControl
		 */

		public VControl getCustomControl() {
			return customControl;
		}

	}

	private static final String BUNDLE_ID = "org.eclipse.emf.ecp.view.custom.ui.swt.test";

	/**
	 * @throws EMFFormsNoRendererException
	 *
	 */
	// FIXME what to expect
	@Test
	@Ignore
	public void testCustomControlinView() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		final VElement controlInView = createCustomControlInView();
		final Shell shell = SWTViewTestHelper.createShell();
		final Composite composite = (Composite) SWTViewTestHelper.render(controlInView, shell);
		assertSame(composite, CustomControlStub.getParent());
	}

	/**
	 * @return
	 */
	private VElement createCustomControlInView() {
		final VView view = VViewFactory.eINSTANCE.createView();

		final VCustomControl customControl = createCustomControl();

		view.getChildren().add(customControl);
		customControl.setBundleName(BUNDLE_ID);
		customControl.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub");
		// TODO check id
		final VCustomDomainModelReference domainModelReference = VCustomFactory.eINSTANCE
			.createCustomDomainModelReference();
		domainModelReference.setBundleName(BUNDLE_ID);
		domainModelReference.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub");
		customControl.setDomainModelReference(domainModelReference);
		return view;
	}

	@Test
	public void testCustomControlinViewWithoutClass() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		final VView view = VViewFactory.eINSTANCE.createView();

		final VCustomControl customControl = createCustomControl();

		view.getChildren().add(customControl);
		customControl.setBundleName(BUNDLE_ID);
		customControl.setClassName("org.eclipse.emf.ecp.view.customcomposite.ui.swt.test.NoExisting");
		// TODO check id
		final VCustomDomainModelReference domainModelReference = VCustomFactory.eINSTANCE
			.createCustomDomainModelReference();
		domainModelReference.setBundleName(BUNDLE_ID);
		domainModelReference.setClassName("org.eclipse.emf.ecp.view.customcomposite.ui.swt.test.NoExisting");
		customControl.setDomainModelReference(domainModelReference);
		// setup ui
		final Shell shell = SWTViewTestHelper.createShell();
		SWTViewTestHelper.render(view, shell);
		assertEquals(1, shell.getChildren().length);
		assertTrue(Composite.class.isInstance(shell.getChildren()[0]));
		assertEquals(0, Composite.class.cast(shell.getChildren()[0]).getChildren().length);
	}

	@Test
	public void testCustomControlInit() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		final VView view = VViewFactory.eINSTANCE.createView();

		final VCustomControl customControl = createCustomControl();

		view.getChildren().add(customControl);
		customControl.setBundleName(BUNDLE_ID);
		customControl.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub2");
		// TODO check id
		final VCustomDomainModelReference domainModelReference = VCustomFactory.eINSTANCE
			.createCustomDomainModelReference();
		domainModelReference.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub2");
		domainModelReference.setBundleName(BUNDLE_ID);
		customControl.setDomainModelReference(domainModelReference);

		final Fan domainModel = BowlingFactory.eINSTANCE.createFan();
		final Shell shell = SWTViewTestHelper.createShell();
		SWTViewTestHelper.render(view, domainModel, shell);
		// ViewModelContextFactory.INSTANCE.createViewModelContext(view, domainModel);

		assertNotNull(domainModel.getFavouriteMerchandise());
	}

	/**
	 * @return
	 */

	private VCustomControl createCustomControl() {
		return VCustomFactory.eINSTANCE.createCustomControl();

	}
}
