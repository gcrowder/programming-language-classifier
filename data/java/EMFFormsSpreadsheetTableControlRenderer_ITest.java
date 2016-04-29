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
package org.eclipse.emfforms.internal.spreadsheet.core.renderer.table;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.makeithappen.model.task.Task;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskFactory;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskPackage;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewModelProperties;
import org.eclipse.emf.ecp.view.spi.table.model.DetailEditing;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.spreadsheet.core.transfer.EMFFormsSpreadsheetExporter;
import org.eclipse.emfforms.spi.spreadsheet.core.transfer.EMFFormsSpreadsheetImporter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class EMFFormsSpreadsheetTableControlRenderer_ITest {
	private DefaultRealm realm;

	@Before
	public void setup() {
		realm = new DefaultRealm();
	}

	@After
	public void tearDown() {
		realm.dispose();
	}

	@Test
	public void test() throws DatatypeConfigurationException, DatabindingFailedException, IOException {
		// write data
		@SuppressWarnings("restriction")
		final EMFFormsSpreadsheetExporter viewRenderer = new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl(
			new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl.ViewProvider() {
				@Override
				public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
					return getView(DetailEditing.NONE);
				}
			});
		final EObject domainModel = getDomainModel();
		final EObject domainModel2 = getDomainModel();
		final Workbook workbook = viewRenderer.render(Arrays.asList(domainModel, domainModel2), null, null);

		final Sheet sheet = workbook.getSheet("root"); //$NON-NLS-1$
		assertEquals(4, sheet.getLastRowNum()); // the rows 0,1,2 are fix and then 3,4 are added
		assertEquals(22, sheet.getRow(0).getLastCellNum());// there are 22 rows, (21 from the view model + 1 for the id)
		// read data

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> domainModels = spreadsheetImport.importSpreadsheet(workbook,
			TaskPackage.eINSTANCE.getTask()).getImportedEObjects();
		assertEquals(2, domainModels.size());
		for (final EObject model : domainModels) {
			assertTrue(EcoreUtil.equals(model, domainModel));
		}
	}

	@Test
	public void testWithDialogDetail() throws DatatypeConfigurationException, DatabindingFailedException, IOException {
		// write data
		@SuppressWarnings("restriction")
		final EMFFormsSpreadsheetExporter viewRenderer = new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl(
			new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl.ViewProvider() {
				@Override
				public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
					return getView(DetailEditing.WITH_DIALOG);
				}
			});
		final EObject domainModel = getDomainModel();
		final EObject domainModel2 = getDomainModel();
		final Workbook workbook = viewRenderer.render(Arrays.asList(domainModel, domainModel2), null, null);

		final Sheet sheet = workbook.getSheet("root"); //$NON-NLS-1$
		assertEquals(4, sheet.getLastRowNum()); // the rows 0,1,2 are fix and then 3,4 are added
		assertEquals(22, sheet.getRow(0).getLastCellNum());// there are 22 rows
		// read data

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> domainModels = spreadsheetImport.importSpreadsheet(workbook,
			TaskPackage.eINSTANCE.getTask()).getImportedEObjects();

		assertEquals(2, domainModels.size());

		for (final EObject model : domainModels) {
			assertTrue(EcoreUtil.equals(model, domainModel));
		}
	}

	private EObject getDomainModel() {
		final Task task = TaskFactory.eINSTANCE.createTask();
		for (int i = 0; i < 3; i++) {
			final Task subTask = TaskFactory.eINSTANCE.createTask();
			subTask.setName("task_" + i); //$NON-NLS-1$
			task.getSubTasks().add(subTask);

			for (int k = 0; k < 3; k++) {
				final Task subsubTask = TaskFactory.eINSTANCE.createTask();
				subsubTask.setDescription(String.format("bla_%1$s_%2$s", i, k)); //$NON-NLS-1$
				subsubTask.setName(String.format("task__%1$s_%2$s", i, k)); //$NON-NLS-1$
				subTask.getSubTasks().add(subsubTask);
			}
		}
		return task;
	}

	private VView getView(DetailEditing detailEditing) {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TaskPackage.eINSTANCE.getTask());
		final VTableControl table = VTableFactory.eINSTANCE.createTableControl();
		table.setDetailEditing(detailEditing);
		view.getChildren().add(table);

		final VTableDomainModelReference tDMR = VTableFactory.eINSTANCE.createTableDomainModelReference();
		table.setDomainModelReference(tDMR);
		final VFeaturePathDomainModelReference innerDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		innerDMR.setDomainModelEFeature(TaskPackage.eINSTANCE.getTask_SubTasks());
		tDMR.setDomainModelReference(innerDMR);
		final VFeaturePathDomainModelReference col1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		col1.setDomainModelEFeature(TaskPackage.eINSTANCE.getTask_Name());
		tDMR.getColumnDomainModelReferences().add(col1);

		final VView subView = VViewFactory.eINSTANCE.createView();
		subView.setRootEClass(TaskPackage.eINSTANCE.getTask());

		final VTableControl subtable = VTableFactory.eINSTANCE.createTableControl();
		subView.getChildren().add(subtable);
		final VTableDomainModelReference subtDMR = VTableFactory.eINSTANCE.createTableDomainModelReference();
		subtable.setDomainModelReference(subtDMR);
		final VFeaturePathDomainModelReference subinnerDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		subinnerDMR.setDomainModelEFeature(TaskPackage.eINSTANCE.getTask_SubTasks());
		subtDMR.setDomainModelReference(subinnerDMR);
		final VFeaturePathDomainModelReference subcol1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		subcol1.setDomainModelEFeature(TaskPackage.eINSTANCE.getTask_Name());
		subtDMR.getColumnDomainModelReferences().add(subcol1);

		final VView subsubView = VViewFactory.eINSTANCE.createView();
		subsubView.setRootEClass(TaskPackage.eINSTANCE.getTask());

		final VControl vControl = VViewFactory.eINSTANCE.createControl();
		subsubView.getChildren().add(vControl);
		final VFeaturePathDomainModelReference col2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		col2.setDomainModelEFeature(TaskPackage.eINSTANCE.getTask_Description());
		vControl.setDomainModelReference(col2);

		subtable.setDetailEditing(DetailEditing.WITH_PANEL);
		subtable.setDetailView(subsubView);

		table.setDetailEditing(DetailEditing.WITH_PANEL);
		table.setDetailView(subView);
		return view;
	}
}
