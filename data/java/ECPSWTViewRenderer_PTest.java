/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas - initial API and implementation
 *******************************************************************************/
package org.eclipse.emf.ecp.ui.view.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTView;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Jonas
 *
 */
@RunWith(DatabindingClassRunner.class)
public class ECPSWTViewRenderer_PTest {

	private static EObject domainObject;
	private static Shell shell;
	private static VView view;

	@Before
	public void init() {
		// setup model
		domainObject = createDomainObject();
		shell = SWTViewTestHelper.createShell();
		view = VViewFactory.eINSTANCE.createView();
	}

	@Test
	public void testEmptyView() throws ECPRendererException {

		final ECPSWTView swtView = ECPSWTViewRenderer.INSTANCE.render(shell, domainObject, view);

		final Control control = swtView.getSWTControl();
		checkFirstLevel(control);
	}

	private void checkFirstLevel(final Control control) {
		assertEquals("The Composite for the View has not been rendered", 1,
			shell.getChildren().length);
		assertTrue("View was not rendered as Composite",
			shell.getChildren()[0] instanceof Composite);
		assertEquals("Returned control and rendered control are not the same",
			control, shell.getChildren()[0]);
		assertTrue(control instanceof Composite);
	}

	private static Player createDomainObject() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		player.setName("Test");
		return player;
	}

	@Test
	public void testViewWithControls() throws ECPRendererException {
		final org.eclipse.emf.ecp.view.spi.model.VControl control = VViewFactory.eINSTANCE.createControl();
		assignPlayerNameFeature(control);
		view.getChildren().add(control);

		final ECPSWTView swtView = ECPSWTViewRenderer.INSTANCE.render(shell, domainObject, view);

		final Control swtControl = swtView.getSWTControl();
		checkFirstLevel(swtControl);
		final Composite composite = (Composite) swtControl;
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControlWithLabel(composite));
	}

	private void assignPlayerNameFeature(final org.eclipse.emf.ecp.view.spi.model.VControl control) {
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		control.setDomainModelReference(domainModelReference);
	}

	@Test
	/**
	 * Adds 2 controls and set them as readonly
	 */
	public void testAllReadOnly() throws ECPRendererException {
		final org.eclipse.emf.ecp.view.spi.model.VControl control = VViewFactory.eINSTANCE.createControl();
		assignPlayerNameFeature(control);
		final org.eclipse.emf.ecp.view.spi.model.VControl control1 = VViewFactory.eINSTANCE.createControl();
		assignPlayerNameFeature(control1);

		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(column);
		column.getChildren().add(control);
		column.getChildren().add(control1);

		view.setAllContentsReadOnly();

		assertTrue(control.isReadonly());
		assertTrue(control1.isReadonly());

		final ECPSWTView swtView = ECPSWTViewRenderer.INSTANCE.render(shell, domainObject, view);
		final Control swtControl = swtView.getSWTControl();
		assertTrue(swtControl instanceof Composite);

		final Composite composite = (Composite) swtControl;

		final List<Text> textFields = getTextField(composite);

		assertEquals(2, textFields.size());
		for (final Text text : textFields) {
			assertTrue(!text.getEnabled());
		}

	}

	/**
	 * @param composite
	 * @return
	 */
	private List<Text> getTextField(Composite composite) {
		return SWTViewTestHelper.getAllTextControls(composite);
	}

	@Test
	/**
	 * set Empty View as ReadOnly
	 */
	public void testViewWithOneContainerReadOnly() {

		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(column);
		view.setAllContentsReadOnly();
		assertTrue(view.isReadonly());
		assertTrue(column.isReadonly());

	}

	@Test
	/**
	 * set Empty View as ReadOnly
	 */
	public void testEmptyViewReadOnly() {

		view.setAllContentsReadOnly();
		// if we reach here, it means there is no error or exception
		assertTrue(view.isReadonly());

	}

}
