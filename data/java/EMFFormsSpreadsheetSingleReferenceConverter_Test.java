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
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.makeithappen.model.task.Task;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskFactory;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskPackage;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsCellStyleConstants;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsConverterException;
import org.eclipse.emfforms.spi.spreadsheet.core.converter.EMFFormsSpreadsheetValueConverter;
import org.junit.Before;
import org.junit.Test;

public class EMFFormsSpreadsheetSingleReferenceConverter_Test {
	private static final String LINE_SEP = System.getProperty("line.separator"); //$NON-NLS-1$
	private static final String EXPECTED = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + LINE_SEP + //$NON-NLS-1$
		"<org.eclipse.emf.ecp.makeithappen.model.task:Task xmi:version=\"2.0\" xmlns:xmi=\"http://www.omg.org/XMI\" xmlns:org.eclipse.emf.ecp.makeithappen.model.task=\"http://eclipse/org/emf/ecp/makeithappen/model/task\" description=\"1\"/>" //$NON-NLS-1$
		+ LINE_SEP;

	private ReportService reportService;
	private EMFFormsDatabindingEMF databinding;
	private EMFFormsSpreadsheetSingleReferenceConverter converter;
	private EObject domainObject;
	private VDomainModelReference dmr;
	private Cell cell;
	private ViewModelContext viewModelContext;

	@Before
	public void before() {
		converter = new EMFFormsSpreadsheetSingleReferenceConverter();
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
	public void testApplicableNoReference() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(TaskPackage.eINSTANCE.getUser_Active());
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenReturn(setting);
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testApplicableMultiReference() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(TaskPackage.eINSTANCE.getTask_SubTasks());
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenReturn(setting);
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(EMFFormsSpreadsheetValueConverter.NOT_APPLICABLE, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testApplicableSingleReference() throws DatabindingFailedException {
		final Setting setting = mock(Setting.class);
		when(setting.getEStructuralFeature()).thenReturn(TaskPackage.eINSTANCE.getTask_Assignee());
		when(databinding.getSetting(any(VDomainModelReference.class), any(EObject.class)))
			.thenReturn(setting);
		converter.setDatabinding(databinding);
		converter.setReportService(reportService);
		assertEquals(0d, converter.isApplicable(domainObject, dmr), 0d);
	}

	@Test
	public void testToString() throws DatabindingFailedException, EMFFormsConverterException {
		final EObject cellValue = task("1"); //$NON-NLS-1$
		final EStructuralFeature eStructuralFeature = TaskPackage.eINSTANCE.getTask_Assignee();
		converter.setCellValue(cell, cellValue, eStructuralFeature, viewModelContext);
		assertEquals(EXPECTED, cell.getStringCellValue());
	}

	@Test
	public void testFromString() throws DatabindingFailedException, EMFFormsConverterException {
		final EObject cellValue = task("1"); //$NON-NLS-1$
		cell.setCellValue(EXPECTED);
		final EStructuralFeature eStructuralFeature = TaskPackage.eINSTANCE.getTask_Assignee();
		final Object value = converter.getCellValue(cell, eStructuralFeature);
		assertTrue(EcoreUtil.equals(cellValue, (EObject) value));
	}

	private static Task task(String desc) {
		final Task task = TaskFactory.eINSTANCE.createTask();
		task.setDescription(desc);
		return task;

	}

}
