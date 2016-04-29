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
package org.eclipse.emfforms.internal.spreadsheet.core.renderer.categorization;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.WorkbookUtil;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationFactory;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.internal.spreadsheet.core.EMFFormsSpreadsheetViewModelContext;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsAbstractSpreadsheetRenderer;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsSpreadsheetRenderTarget;
import org.eclipse.emfforms.spi.spreadsheet.core.EMFFormsSpreadsheetRendererFactory;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.junit.Test;
import org.mockito.Matchers;
import org.mockito.Mockito;

/**
 * Test class for all spreadsheet categorization renderer.
 *
 * @author Eugen Neufeld
 */
@SuppressWarnings({ "unchecked", "restriction" })
public class SpreadsheetCategorization_Test {

	@Test
	public void testEMFFormsCategorizationElementRenderer() throws EMFFormsNoRendererException {
		final Workbook workbook = new HSSFWorkbook();
		final EMFFormsSpreadsheetRendererFactory rendererFactory = Mockito
			.mock(EMFFormsSpreadsheetRendererFactory.class);
		final ReportService reportService = Mockito.mock(ReportService.class);
		final EMFFormsCategorizationElementRenderer renderer = new EMFFormsCategorizationElementRenderer(
			rendererFactory, reportService);
		final ViewModelContext viewModelContext = new EMFFormsSpreadsheetViewModelContext(
			VViewFactory.eINSTANCE.createView(), null);
		final EMFFormsSpreadsheetRenderTarget renderTarget = new EMFFormsSpreadsheetRenderTarget("root", 0, 0); //$NON-NLS-1$

		final VCategorizationElement categorizationElement = VCategorizationFactory.eINSTANCE
			.createCategorizationElement();
		final VCategory category1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VCategory category2 = VCategorizationFactory.eINSTANCE.createCategory();
		categorizationElement.getCategorizations().add(category1);
		categorizationElement.getCategorizations().add(category2);

		final EMFFormsAbstractSpreadsheetRenderer<VElement> categoryRenderer = Mockito
			.mock(EMFFormsAbstractSpreadsheetRenderer.class);
		Mockito.when(rendererFactory.getRendererInstance(category1, viewModelContext)).thenReturn(categoryRenderer);
		Mockito.when(rendererFactory.getRendererInstance(category2, viewModelContext)).thenReturn(categoryRenderer);

		renderer.render(workbook, categorizationElement, viewModelContext, renderTarget);
		Mockito.verify(rendererFactory).getRendererInstance(category1, viewModelContext);
		Mockito.verify(rendererFactory).getRendererInstance(category2, viewModelContext);
		Mockito.verify(categoryRenderer).render(workbook, category1, viewModelContext, renderTarget);
		Mockito.verify(categoryRenderer).render(workbook, category2, viewModelContext, renderTarget);
	}

	@Test
	public void testEMFFormsCategorizationRenderer() throws EMFFormsNoRendererException {
		final Workbook workbook = new HSSFWorkbook();
		final EMFFormsSpreadsheetRendererFactory rendererFactory = Mockito
			.mock(EMFFormsSpreadsheetRendererFactory.class);
		final ReportService reportService = Mockito.mock(ReportService.class);
		final EMFFormsCategorizationRenderer renderer = new EMFFormsCategorizationRenderer(rendererFactory,
			reportService);
		final ViewModelContext viewModelContext = new EMFFormsSpreadsheetViewModelContext(
			VViewFactory.eINSTANCE.createView(), null);
		final EMFFormsSpreadsheetRenderTarget renderTarget = new EMFFormsSpreadsheetRenderTarget("root", 0, 0); //$NON-NLS-1$
		final VCategorization categorization = VCategorizationFactory.eINSTANCE.createCategorization();
		final VCategory category1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VCategory category2 = VCategorizationFactory.eINSTANCE.createCategory();
		categorization.getCategorizations().add(category1);
		categorization.getCategorizations().add(category2);

		final EMFFormsAbstractSpreadsheetRenderer<VElement> categoryRenderer = Mockito
			.mock(EMFFormsAbstractSpreadsheetRenderer.class);
		Mockito.when(rendererFactory.getRendererInstance(category1, viewModelContext)).thenReturn(categoryRenderer);
		Mockito.when(rendererFactory.getRendererInstance(category2, viewModelContext)).thenReturn(categoryRenderer);

		renderer.render(workbook, categorization, viewModelContext, renderTarget);
		Mockito.verify(rendererFactory).getRendererInstance(category1, viewModelContext);
		Mockito.verify(rendererFactory).getRendererInstance(category2, viewModelContext);
		Mockito.verify(categoryRenderer).render(workbook, category1, viewModelContext, renderTarget);
		Mockito.verify(categoryRenderer).render(workbook, category2, viewModelContext, renderTarget);
	}

	@Test
	public void testEMFFormsCategoryRenderer() throws EMFFormsNoRendererException {
		final Workbook workbook = new HSSFWorkbook();
		final EMFFormsSpreadsheetRendererFactory rendererFactory = Mockito
			.mock(EMFFormsSpreadsheetRendererFactory.class);
		final ReportService reportService = Mockito.mock(ReportService.class);
		final EMFFormsCategoryRenderer renderer = new EMFFormsCategoryRenderer(rendererFactory, reportService);
		final ViewModelContext viewModelContext = new EMFFormsSpreadsheetViewModelContext(
			VViewFactory.eINSTANCE.createView(), null);
		final EMFFormsSpreadsheetRenderTarget renderTarget = new EMFFormsSpreadsheetRenderTarget("root", 0, 0); //$NON-NLS-1$
		final VCategory category = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl vElement = VViewFactory.eINSTANCE.createControl();
		category.setComposite(vElement);

		final EMFFormsAbstractSpreadsheetRenderer<VElement> categoryRenderer = Mockito
			.mock(EMFFormsAbstractSpreadsheetRenderer.class);
		Mockito.when(rendererFactory.getRendererInstance(vElement, viewModelContext)).thenReturn(categoryRenderer);

		renderer.render(workbook, category, viewModelContext, renderTarget);

		Mockito.verify(rendererFactory).getRendererInstance(vElement, viewModelContext);
		Mockito.verify(categoryRenderer).render(Matchers.same(workbook), Matchers.same(vElement),
			Matchers.same(viewModelContext), Matchers.argThat(new BaseMatcher<EMFFormsSpreadsheetRenderTarget>() {

				@Override
				public boolean matches(Object item) {
					if (!EMFFormsSpreadsheetRenderTarget.class.isInstance(item)) {
						return false;
					}
					final EMFFormsSpreadsheetRenderTarget target = EMFFormsSpreadsheetRenderTarget.class.cast(item);
					if (0 != target.getColumn()) {
						return false;
					}
					if (0 != target.getRow()) {
						return false;
					}

					final String expected = WorkbookUtil
						.createSafeSheetName(workbook.getNumberOfSheets() + 1 + " " + vElement.getLabel()); //$NON-NLS-1$
					if (!expected.equals(target.getSheetName())) {
						return false;
					}
					return true;
				}

				@Override
				public void describeTo(Description description) {
				}
			}));
	}

}
