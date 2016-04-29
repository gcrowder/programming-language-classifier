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

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFFormsDIRendererFactory}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsDIRendererFactory_Test {

	private EMFFormsDIRendererFactory diRendererFactory;

	/**
	 * Creates a new {@link EMFFormsDIRendererFactory} for every test case.
	 */
	@Before
	public void setUp() {
		diRendererFactory = new EMFFormsDIRendererFactory();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.di.EMFFormsDIRendererFactory#isApplicable(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testIsApplicableOneApplicable() {
		final EMFFormsDIRendererService<VElement> diRendererService1 = mock(EMFFormsDIRendererService.class);
		when(diRendererService1.isApplicable(any(VElement.class), any(ViewModelContext.class))).thenReturn(
			EMFFormsDIRendererService.NOT_APPLICABLE);

		final EMFFormsDIRendererService<VElement> diRendererService2 = mock(EMFFormsDIRendererService.class);
		when(diRendererService2.isApplicable(any(VElement.class), any(ViewModelContext.class))).thenReturn(10.0);

		diRendererFactory.addEMFFormsDIRendererService(diRendererService1);
		diRendererFactory.addEMFFormsDIRendererService(diRendererService2);

		assertEquals(10.0, diRendererFactory.isApplicable(mock(VElement.class), mock(ViewModelContext.class)), 0.0);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.di.EMFFormsDIRendererFactory#isApplicable(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testIsApplicableNoneApplicable() {
		final EMFFormsDIRendererService<VElement> diRendererService1 = mock(EMFFormsDIRendererService.class);
		when(diRendererService1.isApplicable(any(VElement.class), any(ViewModelContext.class))).thenReturn(
			EMFFormsDIRendererService.NOT_APPLICABLE);

		final EMFFormsDIRendererService<VElement> diRendererService2 = mock(EMFFormsDIRendererService.class);
		when(diRendererService2.isApplicable(any(VElement.class), any(ViewModelContext.class))).thenReturn(
			EMFFormsDIRendererService.NOT_APPLICABLE);

		diRendererFactory.addEMFFormsDIRendererService(diRendererService1);
		diRendererFactory.addEMFFormsDIRendererService(diRendererService2);

		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE,
			diRendererFactory.isApplicable(mock(VElement.class), mock(ViewModelContext.class)), 0.0);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.spi.swt.core.di.EMFFormsDIRendererFactory#isApplicable(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 */
	@SuppressWarnings("unchecked")
	@Test
	public void testIsApplicableTwoApplicable() {
		final EMFFormsDIRendererService<VElement> diRendererService1 = mock(EMFFormsDIRendererService.class);
		when(diRendererService1.isApplicable(any(VElement.class), any(ViewModelContext.class))).thenReturn(2.0);

		final EMFFormsDIRendererService<VElement> diRendererService2 = mock(EMFFormsDIRendererService.class);
		when(diRendererService2.isApplicable(any(VElement.class), any(ViewModelContext.class))).thenReturn(10.0);

		diRendererFactory.addEMFFormsDIRendererService(diRendererService1);
		diRendererFactory.addEMFFormsDIRendererService(diRendererService2);

		assertEquals(10.0, diRendererFactory.isApplicable(mock(VElement.class), mock(ViewModelContext.class)), 0.0);
	}

}
