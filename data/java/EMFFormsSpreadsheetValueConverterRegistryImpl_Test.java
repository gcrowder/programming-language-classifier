/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.spreadsheet.core.converter;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsConverterException;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsSpreadsheetValueConverter;
import org.junit.Test;

public class EMFFormsSpreadsheetValueConverterRegistryImpl_Test {

	@Test
	public void testAddConverter() throws EMFFormsConverterException {
		final EMFFormsSpreadsheetValueConverterRegistryImpl registry = new EMFFormsSpreadsheetValueConverterRegistryImpl();
		final EObject object = mock(EObject.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		boolean exception = false;
		try {
			registry.getConverter(object, dmr);
		} catch (final EMFFormsConverterException ex) {
			exception = true;
		}
		assertTrue(exception);
		final EMFFormsSpreadsheetValueConverter converter = mock(EMFFormsSpreadsheetValueConverter.class);
		when(converter.isApplicable(any(EObject.class), any(VDomainModelReference.class))).thenReturn(0d);
		registry.addConverter(converter);
		assertSame(converter, registry.getConverter(object, dmr));
	}

	@Test(expected = EMFFormsConverterException.class)
	public void testRemoveConverter() throws EMFFormsConverterException {
		final EMFFormsSpreadsheetValueConverterRegistryImpl registry = new EMFFormsSpreadsheetValueConverterRegistryImpl();
		final EObject object = mock(EObject.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		final EMFFormsSpreadsheetValueConverter converter = mock(EMFFormsSpreadsheetValueConverter.class);
		when(converter.isApplicable(any(EObject.class), any(VDomainModelReference.class))).thenReturn(0d);
		registry.addConverter(converter);
		try {
			assertSame(converter, registry.getConverter(object, dmr));
		} catch (final EMFFormsConverterException ex1) {
			fail(ex1.getMessage());
		}
		registry.removeConverter(converter);
		registry.getConverter(object, dmr);
	}

	@Test
	public void testGetConverterOneApplicableOneNotApplicable() throws EMFFormsConverterException {
		final EMFFormsSpreadsheetValueConverterRegistryImpl registry = new EMFFormsSpreadsheetValueConverterRegistryImpl();
		final EObject object = mock(EObject.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		final EMFFormsSpreadsheetValueConverter applicable = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable.isApplicable(object, dmr)).thenReturn(0d);
		registry.addConverter(applicable);
		final EMFFormsSpreadsheetValueConverter notApplicable = mock(EMFFormsSpreadsheetValueConverter.class);
		when(notApplicable.isApplicable(object, dmr)).thenReturn(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE);
		registry.addConverter(notApplicable);
		assertSame(applicable, registry.getConverter(object, dmr));
	}

	@Test
	public void testGetConverterMutipleApplicable() throws EMFFormsConverterException {
		final EMFFormsSpreadsheetValueConverterRegistryImpl registry = new EMFFormsSpreadsheetValueConverterRegistryImpl();
		final EObject object = mock(EObject.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		final EMFFormsSpreadsheetValueConverter applicable1 = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable1.isApplicable(object, dmr)).thenReturn(1d);
		registry.addConverter(applicable1);
		final EMFFormsSpreadsheetValueConverter applicable0 = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable0.isApplicable(object, dmr)).thenReturn(0d);
		registry.addConverter(applicable0);
		assertSame(applicable1, registry.getConverter(object, dmr));
	}

	@Test(expected = EMFFormsConverterException.class)
	public void testGetConverterNoApplicable() throws EMFFormsConverterException {
		final EMFFormsSpreadsheetValueConverterRegistryImpl registry = new EMFFormsSpreadsheetValueConverterRegistryImpl();
		final EObject object = mock(EObject.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		final EMFFormsSpreadsheetValueConverter applicable1 = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable1.isApplicable(object, dmr)).thenReturn(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE);
		registry.addConverter(applicable1);
		final EMFFormsSpreadsheetValueConverter applicable0 = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable0.isApplicable(object, dmr)).thenReturn(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE);
		registry.addConverter(applicable0);
		assertSame(applicable1, registry.getConverter(object, dmr));
	}

	@Test(expected = EMFFormsConverterException.class)
	public void testGetConverterMultipleSamePrio() throws EMFFormsConverterException {
		final EMFFormsSpreadsheetValueConverterRegistryImpl registry = new EMFFormsSpreadsheetValueConverterRegistryImpl();
		final EObject object = mock(EObject.class);
		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		final EMFFormsSpreadsheetValueConverter applicable1 = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable1.isApplicable(object, dmr)).thenReturn(0d);
		registry.addConverter(applicable1);
		final EMFFormsSpreadsheetValueConverter applicable0 = mock(EMFFormsSpreadsheetValueConverter.class);
		when(applicable0.isApplicable(object, dmr)).thenReturn(0d);
		registry.addConverter(applicable0);
		assertSame(applicable1, registry.getConverter(object, dmr));
	}

}
