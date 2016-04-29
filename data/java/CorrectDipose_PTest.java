/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * EclipseSource Muenchen GmbH - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.core.swt.tests;

import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeListener;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swt.widgets.Shell;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(DatabindingClassRunner.class)
public class CorrectDipose_PTest {

	private VView view;
	private Shell shell;
	private Player domain;
	private ViewModelContext viewContext;

	@Before
	public void before() {
		shell = SWTViewTestHelper.createShell();
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getPlayer());
		domain = BowlingFactory.eINSTANCE.createPlayer();
		final ViewModelContext toSpy = ViewModelContextFactory.INSTANCE.createViewModelContext(view,
			domain);
		viewContext = spy(toSpy);
	}

	@Test
	public void rendererRemovesListenerTest() {
		final VControl vControl = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference vdmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		vdmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		vControl.setDomainModelReference(vdmr);
		view.getChildren().add(vControl);

		try {
			ECPSWTViewRenderer.INSTANCE.render(shell, viewContext);
		} catch (final ECPRendererException e) {
			fail(e.getMessage());
		}
		shell.dispose();

		// Verify that the renderer registered one domain change listener on instantiation and unregistered it on
		// dispose
		verify(viewContext).registerDomainChangeListener(any(ModelChangeListener.class));
		verify(viewContext).unregisterDomainChangeListener(any(ModelChangeListener.class));
	}
}
