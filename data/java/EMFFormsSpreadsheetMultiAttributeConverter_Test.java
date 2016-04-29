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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.BuiltinFormats;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskPackage;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleProvider;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsCellStyleConstants;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsConverterException;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsSpreadsheetValueConverter;
import org.junit.Before;
import org.junit.Test;

public class EMFFormsSpreadsheetMultiAttributeConverter_Test {

	private ReportService reportService;
	private EMFFormsDatabindingEMF databinding;
	private EMFFormsSpreadsheetMultiAttributeConverter converter;
	private EObject domainObject;
	private VDomainModelReference dmr;
	private Cell cell;
	private ViewModelContext viewModelContext;

	@Before
	public void before() {
		converter = new EMFFormsSpreadsheetMultiAttributeConverter();
		reportService = mock(ReportService.class);
		databinding = mock(EMFFormsDatabindingEMF.class);
		domainObject = mock(EObject.class);
		dmr = mock(VDomainModelReference.class);

		final Workbook wb = new HSSFWorkbook();
		final CellStyle cellStyle = wb.createCellStyle();
		cellStyle.setDataFormat((short) BuiltinFormats.getBuiltinFormat("text")); //$NON-NLS-1$

		final Sheet sheet = wb.createSheet("test"); //$NON-NLS-1$

		// Create a row and put some cells in it. Rows are 0 based.
		final Row row = sheet.createRow((short) 0);
		cell = row.createCell(0);

		viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getContextValue(EMFFormsCellStyleConstants.TEXT)).thenReturn(cellStyle);
	}

	@Test
	public void testApplicableNoFeature() throws DatabindingFailedException {
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenThrow(new DatabindingFailedException("")); //$NON-NLS-1$
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testApplicableNoEAttribute() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(TaskPackage.eINSTANCE.getUser_Tasks());
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenReturn(setting);
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testApplicableSingleEAttribute() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(TaskPackage.eINSTANCE.getUser_Active());
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenReturn(setting);
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testApplicableMultiEAttribute() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(BowlingPackage.eINSTANCE.getPlayer_EMails());
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenReturn(setting);
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(0d, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testToStringEmptyList() throws DatabindingFailedException, EMFFormsConverterException {
		final Collection<String> cellValues = Collections.emptyList();
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_EMails();
		converter.setCellValue(cell, cellValues, eStructuralFeature, viewModelContext);
		assertEquals("", cell.getStringCellValue()); //$NON-NLS-1$
	}

	@Test
	public void testToStringNonEmptyList() throws DatabindingFailedException, EMFFormsConverterException {
		final Collection<String> cellValues = Arrays.asList("foo@bar.org", "foo@bar.com"); //$NON-NLS-1$//$NON-NLS-2$
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_EMails();
		converter.setCellValue(cell, cellValues, eStructuralFeature, viewModelContext);
		assertEquals("foo@bar.org foo@bar.com", cell.getStringCellValue()); //$NON-NLS-1$
	}

	@Test
	public void testFromStringEmpty() throws DatabindingFailedException, EMFFormsConverterException {
		final String cellValue = ""; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_EMails();
		final Object value = converter.getCellValue(cell, eStructuralFeature);
		final List<?> list = List.class.cast(value);
		assertTrue(list.isEmpty());
	}

	@Test
	public void testFromStringNull() throws DatabindingFailedException, EMFFormsConverterException {
		final String cellValue = null;
		cell.setCellValue(cellValue);
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_EMails();
		final Object value = converter.getCellValue(cell, eStructuralFeature);
		final List<?> list = List.class.cast(value);
		assertTrue(list.isEmpty());
	}

	@Test
	public void testFromString() throws DatabindingFailedException, EMFFormsConverterException {
		final String cellValue = "foo@bar.org foo@bar.com"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_EMails();
		final Object value = converter.getCellValue(cell, eStructuralFeature);
		final List<?> list = List.class.cast(value);
		assertEquals(2, list.size());
		assertEquals("foo@bar.org", list.get(0)); //$NON-NLS-1$
		assertEquals("foo@bar.com", list.get(1)); //$NON-NLS-1$
	}

	@Test
	public void testDoubleToStringGerman() throws DatabindingFailedException, EMFFormsConverterException {
		final EMFFormsLocaleProvider localeProvider = mock(EMFFormsLocaleProvider.class);
		when(localeProvider.getLocale()).thenReturn(Locale.GERMAN);
		converter.setEMFFormsLocaleProvider(localeProvider);
		final Collection<Double> cellValues = Arrays.asList(1.1, 2.2);
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_Height();
		converter.setCellValue(cell, cellValues, eStructuralFeature, viewModelContext);
		assertEquals("1,1 2,2", cell.getStringCellValue()); //$NON-NLS-1$
	}

	@Test
	public void testDoubleToStringEnglish() throws DatabindingFailedException, EMFFormsConverterException {
		final EMFFormsLocaleProvider localeProvider = mock(EMFFormsLocaleProvider.class);
		when(localeProvider.getLocale()).thenReturn(Locale.ENGLISH);
		converter.setEMFFormsLocaleProvider(localeProvider);
		final Collection<Double> cellValues = Arrays.asList(1.1, 2.2);
		final EStructuralFeature eStructuralFeature = BowlingPackage.eINSTANCE.getPlayer_Height();
		converter.setCellValue(cell, cellValues, eStructuralFeature, viewModelContext);
		assertEquals("1.1 2.2", cell.getStringCellValue()); //$NON-NLS-1$
	}
}
