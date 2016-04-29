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
package org.eclipse.emfforms.internal.spreadsheet.core;

import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsAbstractSpreadsheetRenderer;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsSpreadsheetRendererService;
import org.junit.Test;

/**
 * Test for {@link EMFFormsSpreadsheetRendererFactoryImpl}.
 *
 * @author Eugen Neufeld
 *
 */
public class EMFFormsSpreadsheetRendererFactoryImpl_Test {

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.spreadsheet.core.EMFFormsSpreadsheetRendererFactoryImpl#addEMFFormsSpreadsheetRendererService(org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsSpreadsheetRendererService)}
	 * .
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Test
	public void testAddEMFFormsSpreadsheetRendererService() throws EMFFormsNoRendererException {
		final EMFFormsSpreadsheetRendererFactoryImpl factory = new EMFFormsSpreadsheetRendererFactoryImpl();
		final EMFFormsSpreadsheetRendererService service = mock(EMFFormsSpreadsheetRendererService.class);
		final VElement vElement = mock(VElement.class);
		final ViewModelContext viewModelContext = mock(ViewModelContext.class);
		final EMFFormsAbstractSpreadsheetRenderer renderer = mock(EMFFormsAbstractSpreadsheetRenderer.class);
		when(service.getRendererInstance(vElement, viewModelContext)).thenReturn(renderer);
		factory.addEMFFormsSpreadsheetRendererService(service);
		final EMFFormsAbstractSpreadsheetRenderer<VElement> rendererInstance = factory.getRendererInstance(vElement,
			viewModelContext);
		assertSame(renderer, rendererInstance);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.spreadsheet.core.EMFFormsSpreadsheetRendererFactoryImpl#removeEMFFormsSpreadsheetRendererService(org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsSpreadsheetRendererService)}
	 * .
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Test(expected = EMFFormsNoRendererException.class)
	public void testRemoveEMFFormsSpreadsheetRendererService() throws EMFFormsNoRendererException {
		final EMFFormsSpreadsheetRendererFactoryImpl factory = new EMFFormsSpreadsheetRendererFactoryImpl();
		final EMFFormsSpreadsheetRendererService service = mock(EMFFormsSpreadsheetRendererService.class);
		factory.addEMFFormsSpreadsheetRendererService(service);
		factory.removeEMFFormsSpreadsheetRendererService(service);
		final VElement vElement = mock(VElement.class);
		when(vElement.eClass()).thenReturn(mock(EClass.class));
		factory.getRendererInstance(vElement, mock(ViewModelContext.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.spreadsheet.core.EMFFormsSpreadsheetRendererFactoryImpl#getRendererInstance(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetRendererInstanceNoFittingServicesNullParameters() throws EMFFormsNoRendererException {
		final EMFFormsSpreadsheetRendererFactoryImpl factory = new EMFFormsSpreadsheetRendererFactoryImpl();
		factory.getRendererInstance(null, null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.spreadsheet.core.EMFFormsSpreadsheetRendererFactoryImpl#getRendererInstance(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetRendererInstanceNoFittingServicesNullContext() throws EMFFormsNoRendererException {
		final EMFFormsSpreadsheetRendererFactoryImpl factory = new EMFFormsSpreadsheetRendererFactoryImpl();
		factory.getRendererInstance(mock(VElement.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.spreadsheet.core.EMFFormsSpreadsheetRendererFactoryImpl#getRendererInstance(org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.context.ViewModelContext)}
	 * .
	 *
	 * @throws EMFFormsNoRendererException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetRendererInstanceNoFittingServicesNullElement() throws EMFFormsNoRendererException {
		final EMFFormsSpreadsheetRendererFactoryImpl factory = new EMFFormsSpreadsheetRendererFactoryImpl();
		factory.getRendererInstance(null, mock(ViewModelContext.class));
	}
}
