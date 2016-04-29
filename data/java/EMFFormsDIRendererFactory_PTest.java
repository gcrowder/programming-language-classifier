/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.spi.swt.core.di;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.e4.core.contexts.EclipseContextFactory;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emfforms.internal.swt.core.di.tests.TestControlSWTRendererDI;
import org.eclipse.emfforms.internal.swt.core.di.tests.TestControlSWTRendererServiceDI;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererService;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

/**
 * JUnit plugin tests for {@link EMFFormsDIRendererFactory}.
 *
 * @author Lucas Koehler
 *
 */
@SuppressWarnings("rawtypes")
public class EMFFormsDIRendererFactory_PTest {

	private EMFFormsDIRendererFactory diRendererFactory;
	private ServiceRegistration<EMFFormsDIRendererService> registerRendererService;
	private ServiceReference<EMFFormsRendererService> serviceReference;
	private BundleContext bundleContext;

	private DefaultRealm realm;

	/**
	 * Creates a new {@link EMFFormsDIRendererFactory}, registers the {@link EMFFormsDIRendererService test di
	 * renderer service} and creates a new {@link DefaultRealm} for every test case.
	 */
	@Before
	public void setUp() {
		realm = new DefaultRealm();
		bundleContext = FrameworkUtil.getBundle(EMFFormsDIRendererFactory_PTest.class).getBundleContext();
		registerRendererService = bundleContext.registerService(EMFFormsDIRendererService.class,
			new TestControlSWTRendererServiceDI(), null);
		serviceReference = bundleContext.getServiceReference(EMFFormsRendererService.class);
		diRendererFactory = (EMFFormsDIRendererFactory) bundleContext.getService(serviceReference);
	}

	/**
	 * Clean up
	 */
	@After
	public void tearDown() {
		bundleContext.ungetService(serviceReference);
		registerRendererService.unregister();
		realm.dispose();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.di.EMFFormsDIRendererFactory#getRendererInstance(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testGetRendererInstance() {
		final IEclipseContext eclipseContext = EclipseContextFactory.create("TestContext"); //$NON-NLS-1$
		final ReportService reportService = mock(ReportService.class);
		final EMFFormsDatabinding emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		final EMFFormsLabelProvider emfFormsLabelProvider = mock(EMFFormsLabelProvider.class);
		final VTViewTemplateProvider viewTemplateProvider = mock(VTViewTemplateProvider.class);
		eclipseContext.set(ReportService.class, reportService);
		eclipseContext.set(EMFFormsDatabinding.class, emfFormsDatabinding);
		eclipseContext.set(EMFFormsLabelProvider.class, emfFormsLabelProvider);
		eclipseContext.set(VTViewTemplateProvider.class, viewTemplateProvider);

		final VControl vControl = VViewFactory.eINSTANCE.createControl();
		final EMFFormsContextProvider contextProvider = mock(EMFFormsContextProvider.class);
		when(contextProvider.getContext()).thenReturn(eclipseContext);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getService(EMFFormsContextProvider.class)).thenReturn(contextProvider);

		final AbstractSWTRenderer<VElement> rendererInstance = diRendererFactory.getRendererInstance(vControl,
			viewModelContext);

		assertTrue("The created renderer instance must be of type TestControlSWTRendererDI.", //$NON-NLS-1$
			TestControlSWTRendererDI.class.isInstance(rendererInstance));
	}

}
