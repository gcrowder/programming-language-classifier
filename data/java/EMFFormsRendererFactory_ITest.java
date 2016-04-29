/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.swt.core;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.swt.core.AbstractAdditionalSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsAdditionalRendererService;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererService;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.Mockito;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

/**
 * Integration test for the {@link EMFFormsRendererFactory}.
 *
 * @author Eugen Neufeld
 *
 */
public class EMFFormsRendererFactory_ITest {

	private EMFFormsRendererFactory rendererFactory;
	private ServiceReference<EMFFormsRendererFactory> serviceReference;
	private static BundleContext bundleContext;

	/**
	 * Setup the {@link BundleContext}.
	 */
	@BeforeClass
	public static void beforeClass() {
		final Bundle bundle = FrameworkUtil.getBundle(EMFFormsRendererFactory_ITest.class);
		bundleContext = bundle.getBundleContext();
	}

	/**
	 * Retrieves the {@link EMFFormsRendererFactory} as a service.
	 */
	@Before
	public void setUp() {
		serviceReference = bundleContext.getServiceReference(EMFFormsRendererFactory.class);
		rendererFactory = bundleContext.getService(serviceReference);
	}

	/**
	 * Ungets the service.
	 */
	@After
	public void cleanUp() {
		bundleContext.ungetService(serviceReference);
	}

	/**
	 * Test method for {@link EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * Testing that a registered {@link EMFFormsRendererService} is picked up.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testAddRendererService() throws EMFFormsNoRendererException {
		final VElement vElement = mock(VElement.class);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final AbstractSWTRenderer<VElement> mockedRenderer = new MockedAbstractSWTRenderer(vElement, viewModelContext,
			mock(ReportService.class));
		final EMFFormsRendererService<VElement> rendererService = mock(EMFFormsRendererService.class);
		Mockito.when(rendererService.getRendererInstance(vElement, viewModelContext)).thenReturn(mockedRenderer);
		bundleContext.registerService(EMFFormsRendererService.class, rendererService, null);
		rendererFactory.getRendererInstance(vElement, viewModelContext);
		Mockito.verify(rendererService, Mockito.times(1)).isApplicable(vElement, viewModelContext);
	}

	/**
	 * Test method for {@link EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * Testing that an IllegalStateException is thrown when no {@link EMFFormsRendererService} is registered.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Test(expected = EMFFormsNoRendererException.class)
	public void testRemoveRendererService() throws EMFFormsNoRendererException {
		final EMFFormsRendererService<VElement> rendererService = mock(EMFFormsRendererService.class);
		final ServiceRegistration<EMFFormsRendererService> registerService = bundleContext.registerService(
			EMFFormsRendererService.class, rendererService, null);
		registerService.unregister();
		rendererFactory.getRendererInstance(mock(VElement.class), mock(ViewModelContext.class));
	}

	/**
	 * Test method for {@link EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)}.
	 * Testing that a registered {@link EMFFormsAdditionalRendererService} is picked up.
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testAddAdditionalRendererService() {
		final VElement vElement = mock(VElement.class);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final AbstractAdditionalSWTRenderer<VElement> additionalSWTRenderer = new MockedAbstractAdditionalSWTRenderer(
			vElement, viewModelContext, mock(ReportService.class));
		final EMFFormsAdditionalRendererService<VElement> rendererService = mock(EMFFormsAdditionalRendererService.class);
		Mockito.when(rendererService.getRendererInstances(vElement, viewModelContext)).thenReturn(
			Collections.singleton(additionalSWTRenderer));
		Mockito.when(rendererService.isApplicable(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(true);
		bundleContext.registerService(EMFFormsAdditionalRendererService.class, rendererService, null);
		final Collection<AbstractAdditionalSWTRenderer<VElement>> rendererInstances = rendererFactory
			.getAdditionalRendererInstances(vElement, viewModelContext);
		Mockito.verify(rendererService, Mockito.times(1)).isApplicable(Matchers.any(VElement.class),
			Matchers.any(ViewModelContext.class));
		assertEquals(1, rendererInstances.size());
	}

	/**
	 * Test method for {@link EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)}.
	 * Testing that an IllegalStateException is thrown when no {@link EMFFormsAdditionalRendererService} is registered.
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Test
	public void testRemoveAdditionalRendererService() {
		final EMFFormsAdditionalRendererService<VElement> rendererService = mock(EMFFormsAdditionalRendererService.class);
		final ServiceRegistration<EMFFormsAdditionalRendererService> registerService = bundleContext.registerService(
			EMFFormsAdditionalRendererService.class, rendererService, null);
		registerService.unregister();
		final Collection<AbstractAdditionalSWTRenderer<VElement>> rendererInstances = rendererFactory
			.getAdditionalRendererInstances(mock(VElement.class), mock(ViewModelContext.class));
		assertEquals(0, rendererInstances.size());
	}
}
