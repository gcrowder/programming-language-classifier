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
package org.eclipse.emfforms.internal.swt.core.di.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collection;

import org.eclipse.e4.core.contexts.EclipseContextFactory;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emfforms.internal.swt.core.di.tests.TestControlSWTRendererDI;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererService;
import org.eclipse.emfforms.spi.swt.core.di.EMFFormsContextProvider;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;

/**
 * JUnit plugin test for {@link EMFFormsDIExtensionRendererFactory}.
 *
 * @author Lucas Koehler
 *
 */
@SuppressWarnings("rawtypes")
public class EMFFormsDIExtensionRendererFactory_PTest {

	private EMFFormsDIExtensionRendererFactory diExtensionRendererFactory;
	private ServiceReference<EMFFormsRendererService> serviceReference;
	private BundleContext bundleContext;
	private DefaultRealm realm;

	/**
	 * Gets a {@link EMFFormsDIExtensionRendererFactory} and creates a {@link DefaultRealm} for every test case.
	 *
	 * @throws InvalidSyntaxException does not happen because no actual filter is used
	 */
	@Before
	public void setUp() throws InvalidSyntaxException {
		realm = new DefaultRealm();
		diExtensionRendererFactory = null;
		bundleContext = FrameworkUtil.getBundle(EMFFormsDIExtensionRendererFactory_PTest.class).getBundleContext();
		final Collection<ServiceReference<EMFFormsRendererService>> serviceReferences = bundleContext
			.getServiceReferences(EMFFormsRendererService.class, null);
		for (final ServiceReference<EMFFormsRendererService> currentRef : serviceReferences) {
			final EMFFormsRendererService currentService = bundleContext.getService(currentRef);
			if (EMFFormsDIExtensionRendererFactory.class.isInstance(currentService)) {
				serviceReference = currentRef;
				diExtensionRendererFactory = (EMFFormsDIExtensionRendererFactory) currentService;
				break;
			}
			bundleContext.ungetService(currentRef);
		}
		if (diExtensionRendererFactory == null) {
			fail("No EMFFormsDIExtensionRendererFactory could be found."); //$NON-NLS-1$
		}
	}

	/**
	 * Clean up.
	 */
	@After
	public void tearDown() {
		bundleContext.ungetService(serviceReference);
		realm.dispose();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.di.EMFFormsDIRendererFactory#isApplicable(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		final VControl vControl = mock(VControl.class);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		assertEquals(200L, diExtensionRendererFactory.isApplicable(vControl, viewModelContext), 0.0);
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
		final AbstractSWTRenderer<VElement> rendererInstance = diExtensionRendererFactory.getRendererInstance(vControl,
			viewModelContext);
		assertTrue(TestControlSWTRendererDI.class.isInstance(rendererInstance));
	}

}
