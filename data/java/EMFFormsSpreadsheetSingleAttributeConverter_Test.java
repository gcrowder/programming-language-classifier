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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import javax.xml.datatype.DatatypeConstants;
import javax.xml.datatype.XMLGregorianCalendar;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.BuiltinFormats;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EFactory;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.xml.type.internal.XMLCalendar;
import org.eclipse.emf.ecp.makeithappen.model.task.Gender;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskPackage;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsCellStyleConstants;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsConverterException;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsSpreadsheetValueConverter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class EMFFormsSpreadsheetSingleAttributeConverter_Test {

	private ReportService reportService;
	private EMFFormsDatabindingEMF databinding;
	private EMFFormsSpreadsheetSingleAttributeConverter converter;
	private EObject domainObject;
	private VDomainModelReference dmr;
	private static final TimeZone UTC_TIMEZONE = TimeZone.getTimeZone("UTC"); //$NON-NLS-1$
	private Cell cell;
	private CellStyle cellStyle;
	private ViewModelContext viewModelContext;
	private CellStyle cellStyle2;

	@Before
	public void setup() {
		final Workbook wb = new HSSFWorkbook();
		cellStyle = wb.createCellStyle();
		cellStyle.setDataFormat((short) BuiltinFormats.getBuiltinFormat("text")); //$NON-NLS-1$

		cellStyle2 = wb.createCellStyle();
		cellStyle2.setDataFormat((short) BuiltinFormats.getBuiltinFormat("m/d/yy")); //$NON-NLS-1$

		final Sheet sheet = wb.createSheet("test"); //$NON-NLS-1$

		// Create a row and put some cells in it. Rows are 0 based.
		final Row row = sheet.createRow((short) 0);
		cell = row.createCell(0);

		viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getContextValue(EMFFormsCellStyleConstants.TEXT)).thenReturn(cellStyle);
		when(viewModelContext.getContextValue(EMFFormsCellStyleConstants.DATE)).thenReturn(cellStyle2);

		converter = new EMFFormsSpreadsheetSingleAttributeConverter();
		reportService = mock(ReportService.class);
		databinding = mock(EMFFormsDatabindingEMF.class);
		domainObject = mock(EObject.class);
		dmr = mock(VDomainModelReference.class);
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
	public void testApplicableMultiEAttribute() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(BowlingPackage.eINSTANCE.getPlayer_EMails());
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
		assertEquals(0d, converter.isApplicable(domainObject, dmr), 0d);
	}

	@After
	public void tearDown() {

	}

	@Test
	public void testGetCellValueBooleanPrimitive() throws EMFFormsConverterException {
		final boolean cellValue = true;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBoolean());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Boolean result = (Boolean) value;
		assertEquals(cellValue, result.booleanValue());
	}

	@Test
	public void testGetCellValueBoolean() throws EMFFormsConverterException {
		final Boolean cellValue = false;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBooleanObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Boolean result = (Boolean) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBooleanError() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBooleanObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueBooleanEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBooleanObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueByte() throws EMFFormsConverterException {
		final Byte cellValue = 11;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEByteObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Byte result = (Byte) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBytePrimitive() throws EMFFormsConverterException {
		final byte cellValue = 11;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEByte());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Byte result = (Byte) value;
		assertEquals(cellValue, result.byteValue());
	}

	@Test
	public void testGetCellValueByteErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEByteObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueByteEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEByteObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueShort() throws EMFFormsConverterException {
		final Short cellValue = 111;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEShortObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Short result = (Short) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueShortPrimitive() throws EMFFormsConverterException {
		final short cellValue = 111;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEShort());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Short result = (Short) value;
		assertEquals(cellValue, result.shortValue());
	}

	@Test
	public void testGetCellValueShortErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEShortObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueShortEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEShortObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueInteger() throws EMFFormsConverterException {
		final Integer cellValue = 1111;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEIntegerObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Integer result = (Integer) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueIntegerPrimitive() throws EMFFormsConverterException {
		final int cellValue = 1111;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEInt());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Integer result = (Integer) value;
		assertEquals(cellValue, result.intValue());
	}

	@Test
	public void testGetCellValueIntegerErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEIntegerObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueIntegerEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEIntegerObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueLongPrimitive() throws EMFFormsConverterException {
		final long cellValue = 11111;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getELong());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Long result = (Long) value;
		assertEquals(cellValue, result.longValue());
	}

	@Test
	public void testGetCellValueLong() throws EMFFormsConverterException {
		final Long cellValue = 11111L;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getELongObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Long result = (Long) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueLongErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getELongObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueLongEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getELongObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueFloatPrimitive() throws EMFFormsConverterException {
		final float cellValue = 11.11f;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEFloat());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Float result = (Float) value;
		assertEquals(cellValue, result.floatValue(), 0);
	}

	@Test
	public void testGetCellValueFloat() throws EMFFormsConverterException {
		final Float cellValue = 11.11f;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEFloatObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Float result = (Float) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueFloatErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEFloatObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueFloatEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEFloatObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueDoublePrimitive() throws EMFFormsConverterException {
		final double cellValue = 12.12;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDouble());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Double result = (Double) value;
		assertEquals(cellValue, result.doubleValue(), 0);
	}

	@Test
	public void testGetCellValueDouble() throws EMFFormsConverterException {
		final Double cellValue = 12.12;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDoubleObject());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Double result = (Double) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueDoubleErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDoubleObject());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueDoubleEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDoubleObject());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueBigIntegerString() throws EMFFormsConverterException {
		final BigInteger cellValue = new BigInteger("123456789123456789123456789123465789"); //$NON-NLS-1$
		cell.setCellValue(cellValue.toString());
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigInteger());
		final Object value = converter.getCellValue(cell, eAttribute);
		final BigInteger result = (BigInteger) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBigIntegerDouble() throws EMFFormsConverterException {
		final BigInteger cellValue = new BigInteger("132456789"); //$NON-NLS-1$
		cell.setCellValue(cellValue.doubleValue());
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigInteger());
		final Object value = converter.getCellValue(cell, eAttribute);
		final BigInteger result = (BigInteger) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBigIntegerErrorNumberFormatException() {
		final String cellValue = "1.11"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigInteger());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(NumberFormatException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueBigIntegerEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigInteger());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueBigDecimalString() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal(Double.MAX_VALUE).add(BigDecimal.ONE);
		cell.setCellValue(cellValue.toString());
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		final Object value = converter.getCellValue(cell, eAttribute);
		final BigDecimal result = (BigDecimal) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBigDecimalPrecisionString() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal("12.123456789123456789"); //$NON-NLS-1$
		cell.setCellValue(cellValue.toString());
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		final Object value = converter.getCellValue(cell, eAttribute);
		final BigDecimal result = (BigDecimal) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBigDecimalDouble() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal("123456.789"); //$NON-NLS-1$
		cell.setCellValue(cellValue.doubleValue());
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		final Object value = converter.getCellValue(cell, eAttribute);
		final BigDecimal result = (BigDecimal) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBigDecimalNegativeDouble() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal("-123456.789"); //$NON-NLS-1$
		cell.setCellValue(cellValue.doubleValue());
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		final Object value = converter.getCellValue(cell, eAttribute);
		final BigDecimal result = (BigDecimal) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueBigDecimalErrorNumberFormatException() {
		final String cellValue = "111gg"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(NumberFormatException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueBigDecimalEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueDate() throws EMFFormsConverterException {
		final Date cellValue = new Date();
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDate());
		final Object value = converter.getCellValue(cell, eAttribute);
		final Date result = (Date) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueDateErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDate());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueDateEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDate());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueXmlDate() throws EMFFormsConverterException {
		final XMLGregorianCalendar cellValue = new XMLCalendar(
			Calendar.getInstance(UTC_TIMEZONE).getTime(), XMLCalendar.DATE);
		cellValue.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		cellValue.setHour(DatatypeConstants.FIELD_UNDEFINED);
		cellValue.setMinute(DatatypeConstants.FIELD_UNDEFINED);
		cellValue.setSecond(DatatypeConstants.FIELD_UNDEFINED);
		cellValue.setMillisecond(DatatypeConstants.FIELD_UNDEFINED);

		cell.setCellValue(
			DateUtil.getExcelDate(cellValue.toGregorianCalendar(UTC_TIMEZONE, null, null), false));
		final EDataType dataType = EcoreFactory.eINSTANCE.createEDataType();
		dataType.setInstanceClass(XMLGregorianCalendar.class);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(dataType);
		final Object value = converter.getCellValue(cell, eAttribute);
		final XMLGregorianCalendar result = (XMLGregorianCalendar) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueXmlDateErrorIllegalStateException() {
		final String cellValue = "test"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EDataType dataType = EcoreFactory.eINSTANCE.createEDataType();
		dataType.setInstanceClass(XMLGregorianCalendar.class);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(dataType);
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueXmlDateEmptyCell() throws EMFFormsConverterException {
		final EDataType dataType = EcoreFactory.eINSTANCE.createEDataType();
		dataType.setInstanceClass(XMLGregorianCalendar.class);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(dataType);
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testGetCellValueString() throws EMFFormsConverterException {
		final String cellValue = "asdf"; //$NON-NLS-1$
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEString());
		final Object value = converter.getCellValue(cell, eAttribute);
		final String result = (String) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testGetCellValueStringErrorIllegalStateException() {
		final Double cellValue = 1.1;
		cell.setCellValue(cellValue);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEString());
		try {
			converter.getCellValue(cell, eAttribute);
			fail();
		} catch (final EMFFormsConverterException ex) {
			assertTrue(IllegalStateException.class.isInstance(ex.getCause()));
		}
	}

	@Test
	public void testGetCellValueStringEmptyCell() throws EMFFormsConverterException {
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEString());
		assertNull(converter.getCellValue(cell, eAttribute));
	}

	@Test
	public void testSetCellValueBooleanPrimitive() throws EMFFormsConverterException {
		final boolean cellValue = true;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBoolean());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getBooleanCellValue());
	}

	@Test
	public void testSetCellValueBoolean() throws EMFFormsConverterException {
		final Boolean cellValue = Boolean.FALSE;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBooleanObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getBooleanCellValue());
	}

	@Test
	public void testSetCellValueBytePrimitive() throws EMFFormsConverterException {
		final byte cellValue = 11;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEByte());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueByte() throws EMFFormsConverterException {
		final Byte cellValue = 11;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEByteObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueShortPrimitive() throws EMFFormsConverterException {
		final short cellValue = 111;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEShort());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueShort() throws EMFFormsConverterException {
		final Short cellValue = 111;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEShortObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueIntegerPrimitive() throws EMFFormsConverterException {
		final int cellValue = 1111;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEInt());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueInteger() throws EMFFormsConverterException {
		final Integer cellValue = 1111;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEIntegerObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueLongPrimitive() throws EMFFormsConverterException {
		final long cellValue = 1111L;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getELong());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueLong() throws EMFFormsConverterException {
		final Long cellValue = 1111L;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getELongObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueFloatPrimitive() throws EMFFormsConverterException {
		final float cellValue = 11.11f;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEFloat());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueFloat() throws EMFFormsConverterException {
		final Float cellValue = 11.11f;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEFloatObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueDoublePrimitive() throws EMFFormsConverterException {
		final double cellValue = 12.12d;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDouble());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueDouble() throws EMFFormsConverterException {
		final Double cellValue = 12.12d;
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDoubleObject());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueBigInteger() throws EMFFormsConverterException {
		final BigInteger cellValue = BigInteger.valueOf(Long.MAX_VALUE - 1);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigInteger());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.doubleValue(), cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueBigIntegerText() throws EMFFormsConverterException {
		final BigInteger cellValue = new BigInteger("123456789123456789123456789"); //$NON-NLS-1$
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigInteger());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.toString(), cell.getStringCellValue());
	}

	@Test
	public void testSetCellValueBigDecimal() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal("1234.123456"); //$NON-NLS-1$
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.doubleValue(), cell.getNumericCellValue(), 0);
	}

	@Test
	public void testSetCellValueBigDecimalPrecision() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal("123456789123456789123456789.123456789123456789123456789"); //$NON-NLS-1$
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.toString(), cell.getStringCellValue());
	}

	@Test
	public void testSetCellValueBigDecimalText() throws EMFFormsConverterException {
		final BigDecimal cellValue = new BigDecimal(Double.MAX_VALUE).add(BigDecimal.ONE);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEBigDecimal());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.toString(), cell.getStringCellValue());
	}

	@Test
	public void testSetCellValueDate() throws EMFFormsConverterException {
		final Date cellValue = new Date();
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEDate());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.toString(), cell.getDateCellValue().toString());
	}

	@Test
	public void testSetCellValueXmlDate() throws EMFFormsConverterException {
		final XMLGregorianCalendar cellValue = new XMLCalendar(
			Calendar.getInstance(UTC_TIMEZONE).getTime(), XMLCalendar.DATE);

		final EDataType dataType = EcoreFactory.eINSTANCE.createEDataType();
		dataType.setInstanceClass(XMLGregorianCalendar.class);
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(dataType);

		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.toGregorianCalendar(UTC_TIMEZONE, null, cellValue).getTime(),
			DateUtil.getJavaCalendarUTC(cell.getNumericCellValue(), false).getTime());
	}

	@Test
	public void testSetCellValueString() throws EMFFormsConverterException {
		final String cellValue = "test"; //$NON-NLS-1$
		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(EcorePackage.eINSTANCE.getEString());
		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue, cell.getStringCellValue());
	}

	@Test
	public void testGetCellValueEnum() throws EMFFormsConverterException {
		final Gender cellValue = Gender.MALE;
		cell.setCellValue(cellValue.getLiteral());

		final Object value = converter.getCellValue(cell,
			TaskPackage.eINSTANCE.getUser_Gender());
		final Gender result = (Gender) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testSetCellValueEnum() throws EMFFormsConverterException {
		final Gender cellValue = Gender.MALE;

		converter.setCellValue(cell, cellValue, TaskPackage.eINSTANCE.getUser_Gender(), viewModelContext);
		assertEquals(cellValue.getLiteral(), cell.getStringCellValue());
	}

	@Test
	public void testGetCellValueDynamicEMFEnum() throws EMFFormsConverterException {
		final EEnum eEnum = EcoreFactory.eINSTANCE.createEEnum();
		final EEnumLiteral eEnumLiteral = EcoreFactory.eINSTANCE.createEEnumLiteral();
		final EEnumLiteral eEnumLiteral2 = EcoreFactory.eINSTANCE.createEEnumLiteral();
		eEnum.getELiterals().add(eEnumLiteral);
		eEnum.getELiterals().add(eEnumLiteral2);
		eEnumLiteral.setLiteral("FOO"); //$NON-NLS-1$
		eEnumLiteral.setValue(0);
		eEnumLiteral2.setLiteral("BAR"); //$NON-NLS-1$
		eEnumLiteral2.setValue(1);

		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(eEnum);

		final EPackage ePackage = EcoreFactory.eINSTANCE.createEPackage();
		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		ePackage.getEClassifiers().add(eClass);
		eClass.getEStructuralFeatures().add(eAttribute);
		final EFactory eFactory = EcoreFactory.eINSTANCE.createEFactory();
		ePackage.setEFactoryInstance(eFactory);
		ePackage.getEClassifiers().add(eEnum);

		final EEnumLiteral cellValue = eEnum.getEEnumLiteral((int) Math.round(Math.random()));
		cell.setCellValue(cellValue.getLiteral());
		final Object value = converter.getCellValue(cell, eAttribute);
		final EEnumLiteral result = (EEnumLiteral) value;
		assertEquals(cellValue, result);
	}

	@Test
	public void testSetCellValueDynamicEMFEnum() throws EMFFormsConverterException {
		final EEnum eEnum = EcoreFactory.eINSTANCE.createEEnum();
		final EEnumLiteral eEnumLiteral = EcoreFactory.eINSTANCE.createEEnumLiteral();
		final EEnumLiteral eEnumLiteral2 = EcoreFactory.eINSTANCE.createEEnumLiteral();
		eEnum.getELiterals().add(eEnumLiteral);
		eEnum.getELiterals().add(eEnumLiteral2);
		eEnumLiteral.setLiteral("FOO"); //$NON-NLS-1$
		eEnumLiteral.setValue(0);
		eEnumLiteral2.setLiteral("BAR"); //$NON-NLS-1$
		eEnumLiteral2.setValue(1);

		final EAttribute eAttribute = EcoreFactory.eINSTANCE.createEAttribute();
		eAttribute.setEType(eEnum);

		final EPackage ePackage = EcoreFactory.eINSTANCE.createEPackage();
		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		ePackage.getEClassifiers().add(eClass);
		eClass.getEStructuralFeatures().add(eAttribute);
		final EFactory eFactory = EcoreFactory.eINSTANCE.createEFactory();
		ePackage.setEFactoryInstance(eFactory);
		ePackage.getEClassifiers().add(eEnum);

		final EEnumLiteral cellValue = eEnum.getEEnumLiteral((int) Math.round(Math.random()));

		converter.setCellValue(cell, cellValue, eAttribute, viewModelContext);
		assertEquals(cellValue.getLiteral(), cell.getStringCellValue());
	}

}
