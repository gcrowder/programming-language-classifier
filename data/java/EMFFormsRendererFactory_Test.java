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
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.swt.core.AbstractAdditionalSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsAdditionalRendererService;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererService;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.Mockito;

/**
 * Test for the {@link EMFFormsRendererFactoryImpl}.
 *
 * @author Eugen Neufeld
 *
 */
public class EMFFormsRendererFactory_Test {

	private EMFFormsRendererFactoryImpl rendererFactory;

	/**
	 * Setup the EMFFormsRendererFactoryImpl.
	 */
	@Before
	public void setUp() {
		rendererFactory = new EMFFormsRendererFactoryImpl();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * When no EMFFormsRendererService is registered then an IllegalStateException should be thrown.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@Test(expected = EMFFormsNoRendererException.class)
	public void testGetRendererInstanceNoRendererServices() throws EMFFormsNoRendererException {
		rendererFactory.getRendererInstance(mock(VElement.class), mock(ViewModelContext.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * When no fitting EMFFormsRendererService is available then an IllegalStateException should be thrown.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings("unchecked")
	@Test(expected = EMFFormsNoRendererException.class)
	public void testGetRendererInstanceNoFittingServices() throws EMFFormsNoRendererException {
		final VElement vElement = mock(VElement.class);
		final EMFFormsRendererService<VElement> rendererService1 = mock(EMFFormsRendererService.class);
		when(rendererService1.isApplicable(Matchers.same(vElement), Matchers.any(ViewModelContext.class))).thenReturn(
			1d);
		when(rendererService1.isApplicable(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(
				EMFFormsRendererService.NOT_APPLICABLE);
		final AbstractSWTRenderer<VElement> renderer1 = mock(AbstractSWTRenderer.class);
		when(rendererService1.getRendererInstance(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(renderer1);

		rendererFactory.addEMFFormsRendererService(rendererService1);
		final VElement mockedVElement = mock(VElement.class);
		when(mockedVElement.eClass()).thenReturn(VViewPackage.eINSTANCE.getControl());
		rendererFactory.getRendererInstance(mockedVElement, mock(ViewModelContext.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * When one EMFFormsRendererService is registered then
	 * {@link EMFFormsRendererService#isApplicable(VElement,ViewModelContext)} is
	 * called exactly once.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testAddRendererService() throws EMFFormsNoRendererException {
		final VElement vElement = mock(VElement.class);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final EMFFormsRendererService<VElement> rendererService = mock(EMFFormsRendererService.class);
		when(rendererService.getRendererInstance(vElement, viewModelContext)).thenReturn(
			new MockedAbstractSWTRenderer(vElement, viewModelContext, mock(ReportService.class)));
		rendererFactory.addEMFFormsRendererService(rendererService);
		rendererFactory.getRendererInstance(vElement, viewModelContext);
		Mockito.verify(rendererService, Mockito.times(1)).isApplicable(vElement, viewModelContext);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * When the only EMFFormsRendererService is removed then an IllegalStateException is thrown.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings("unchecked")
	@Test(expected = EMFFormsNoRendererException.class)
	public void testRemoveRendererService() throws EMFFormsNoRendererException {
		final EMFFormsRendererService<VElement> rendererService = mock(EMFFormsRendererService.class);
		rendererFactory.addEMFFormsRendererService(rendererService);
		rendererFactory.removeEMFFormsRendererService(rendererService);
		rendererFactory.getRendererInstance(mock(VElement.class), mock(ViewModelContext.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * Check that the EMFFormsRendererService with the fitting VElement is used to get an AbstractSWTRenderer.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testGetRendererInstanceSamePrioDifferentVElement() throws EMFFormsNoRendererException {
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final VElement vElement1 = Mockito.mock(VElement.class);
		final VElement vElement2 = Mockito.mock(VElement.class);
		final EMFFormsRendererService<VElement> rendererService1 = mock(EMFFormsRendererService.class);
		when(rendererService1.isApplicable(vElement1, viewModelContext)).thenReturn(1d);
		final AbstractSWTRenderer<VElement> renderer1 = new MockedAbstractSWTRenderer(vElement1, viewModelContext,
			mock(ReportService.class));
		when(rendererService1.getRendererInstance(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(renderer1);
		final EMFFormsRendererService<VElement> rendererService2 = mock(EMFFormsRendererService.class);
		when(rendererService2.isApplicable(vElement2, viewModelContext)).thenReturn(1d);
		final AbstractSWTRenderer<VElement> renderer2 = mock(AbstractSWTRenderer.class);
		when(rendererService2.getRendererInstance(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(renderer2);
		rendererFactory.addEMFFormsRendererService(rendererService1);
		rendererFactory.addEMFFormsRendererService(rendererService2);
		assertEquals(renderer1, rendererFactory.getRendererInstance(vElement1, viewModelContext));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getRendererInstance(VElement, ViewModelContext)}.
	 * Check that the EMFFormsRendererService with the higher priority is used to get an AbstractSWTRenderer.
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testGetRendererInstanceDifferentPrioSameVElement() throws EMFFormsNoRendererException {
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final VElement vElement = mock(VElement.class);
		final EMFFormsRendererService<VElement> rendererService1 = mock(EMFFormsRendererService.class);
		when(rendererService1.isApplicable(vElement, viewModelContext)).thenReturn(1d);
		final AbstractSWTRenderer<VElement> renderer1 = new MockedAbstractSWTRenderer(vElement, viewModelContext,
			mock(ReportService.class));
		when(rendererService1.getRendererInstance(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(renderer1);
		final EMFFormsRendererService<VElement> rendererService2 = mock(EMFFormsRendererService.class);
		when(rendererService2.isApplicable(vElement, viewModelContext)).thenReturn(2d);
		final AbstractSWTRenderer<VElement> renderer2 = new MockedAbstractSWTRenderer(vElement, viewModelContext,
			mock(ReportService.class));
		when(rendererService2.getRendererInstance(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(renderer2);
		rendererFactory.addEMFFormsRendererService(rendererService1);
		rendererFactory.addEMFFormsRendererService(rendererService2);
		assertEquals(renderer2, rendererFactory.getRendererInstance(vElement, viewModelContext));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)}.
	 * When one EMFFormsRendererService is registered then
	 * {@link EMFFormsAdditionalRendererService#isApplicable(VElement, ViewModelContext)} is
	 * called exactly once.
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testAddAdditionalRendererService() {
		final VElement vElement = mock(VElement.class);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final EMFFormsAdditionalRendererService<VElement> rendererService = mock(EMFFormsAdditionalRendererService.class);
		when(rendererService.isApplicable(vElement, viewModelContext)).thenReturn(true);
		final Collection<AbstractAdditionalSWTRenderer<VElement>> mockedResults = new ArrayList<AbstractAdditionalSWTRenderer<VElement>>();
		mockedResults
			.add(new MockedAbstractAdditionalSWTRenderer(vElement, viewModelContext, mock(ReportService.class)));
		when(rendererService.getRendererInstances(vElement, viewModelContext)).thenReturn(mockedResults);
		rendererFactory.addEMFFormsAdditionalRendererService(rendererService);
		final Collection<AbstractAdditionalSWTRenderer<VElement>> additionalRenderers = rendererFactory
			.getAdditionalRendererInstances(vElement, viewModelContext);
		Mockito.verify(rendererService, Mockito.times(1)).isApplicable(vElement, viewModelContext);
		assertEquals(1, additionalRenderers.size());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)}.
	 *
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testRemoveAdditionalRendererService() {
		final EMFFormsAdditionalRendererService<VElement> rendererService = mock(EMFFormsAdditionalRendererService.class);
		rendererFactory.addEMFFormsAdditionalRendererService(rendererService);
		rendererFactory.removeEMFFormsAdditionalRendererService(rendererService);
		final Collection<AbstractAdditionalSWTRenderer<VElement>> additionalRenderers = rendererFactory
			.getAdditionalRendererInstances(mock(VElement.class), mock(ViewModelContext.class));
		assertEquals(0, additionalRenderers.size());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)
	 * EMFFormsRendererFactory#getAdditionalRendererInstances(VElement, ViewModelContext)}.
	 * When no fitting EMFFormsRendererService is available then the collection is empty.
	 *
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testGetAdditionalRendererInstanceNoFittingServices() {
		final VElement vElement = mock(VElement.class);
		final EMFFormsAdditionalRendererService<VElement> rendererService1 = mock(EMFFormsAdditionalRendererService.class);
		when(rendererService1.isApplicable(Matchers.same(vElement), Matchers.any(ViewModelContext.class))).thenReturn(
			false);
		final AbstractAdditionalSWTRenderer<VElement> renderer1 = mock(AbstractAdditionalSWTRenderer.class);
		when(rendererService1.getRendererInstances(Matchers.any(VElement.class), Matchers.any(ViewModelContext.class)))
			.thenReturn(Collections.singleton(renderer1));

		rendererFactory.addEMFFormsAdditionalRendererService(rendererService1);
		final Collection<AbstractAdditionalSWTRenderer<VElement>> additionalRenderers = rendererFactory
			.getAdditionalRendererInstances(mock(VElement.class), mock(ViewModelContext.class));
		assertEquals(0, additionalRenderers.size());
	}
}
