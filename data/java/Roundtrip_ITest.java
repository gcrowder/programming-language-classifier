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
package org.eclipse.emfforms.spreadsheet.integrationtest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.datatype.DatatypeConfigurationException;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.xml.type.internal.XMLCalendar;
import org.eclipse.emf.ecp.makeithappen.model.task.Nationality;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskFactory;
import org.eclipse.emf.ecp.makeithappen.model.task.TaskPackage;
import org.eclipse.emf.ecp.makeithappen.model.task.User;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewModelProperties;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emfforms.spi.spreadsheet.core.transfer.EMFFormsSpreadsheetExporter;
import org.eclipse.emfforms.spi.spreadsheet.core.transfer.EMFFormsSpreadsheetImporter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class Roundtrip_ITest {
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
	public void test() throws DatatypeConfigurationException, IOException {
		// write data
		final EMFFormsSpreadsheetExporter viewRenderer = EMFFormsSpreadsheetExporter.INSTANCE;
		final EObject user = getDomainModel();
		final EObject user2 = getDomainModel();
		final VViewModelProperties properties = VViewFactory.eINSTANCE.createViewModelLoadingProperties();
		properties.addNonInheritableProperty("root", true); //$NON-NLS-1$
		properties.addNonInheritableProperty("detail", true); //$NON-NLS-1$
		final Map<String, String> keyValueMap = new LinkedHashMap<String, String>();
		keyValueMap.put("MyColumn1", "MyValue1"); //$NON-NLS-1$ //$NON-NLS-2$
		final Map<String, String> keyValueMap2 = new LinkedHashMap<String, String>();
		keyValueMap2.put("MyColumn2", "MyValue2"); //$NON-NLS-1$ //$NON-NLS-2$

		final Workbook wb = viewRenderer.render(Arrays.asList(user, user2), user, properties);

		final File targetFile = new File("export.xls"); //$NON-NLS-1$
		saveWorkbook(wb, targetFile.getAbsolutePath());

		// read data
		final FileInputStream file = new FileInputStream(targetFile);
		final Workbook workbook = new HSSFWorkbook(file);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> users = spreadsheetImport.importSpreadsheet(workbook,
			TaskPackage.eINSTANCE.getUser()).getImportedEObjects();
		for (final EObject eObject : users) {
			EcoreUtil.equals(eObject, user);
		}

	}

	private void saveWorkbook(Workbook wb, String absolutePath) {
		FileOutputStream fileOut = null;
		try {
			fileOut = new FileOutputStream(absolutePath);
			wb.write(fileOut);
		} catch (final FileNotFoundException e) {
			e.printStackTrace();
		} catch (final IOException e) {
			e.printStackTrace();
		} finally {
			try {
				fileOut.close();
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}
	}

	private EObject getDomainModel() {
		final User user = TaskFactory.eINSTANCE.createUser();
		user.setFirstName("John"); //$NON-NLS-1$
		user.setLastName("Doe"); //$NON-NLS-1$
		user.setEmail("john.doe@mail.com"); //$NON-NLS-1$
		user.setWeight(1.1);
		user.setHeigth(1);
		user.setNationality(Nationality.ITALIAN);
		user.setTimeOfRegistration(Calendar.getInstance().getTime());
		user.setDateOfBirth(new XMLCalendar(Calendar.getInstance().getTime(), XMLCalendar.DATE));
		return user;
	}

	@Test
	public void testUnsetFeatures() throws IOException, DatatypeConfigurationException {

		final Fan domainModel = BowlingFactory.eINSTANCE.createFan();
		domainModel.setNumberOfTournamentsVisited(1);
		assertFalse(domainModel.isSetName());
		assertFalse(domainModel.isSetEMails());
		assertTrue(domainModel.isSetNumberOfTournamentsVisited());
		assertFalse(domainModel.isSetFavouriteMerchandise());
		assertFalse(domainModel.isSetFanMerchandise());

		final VView view = view(BowlingPackage.eINSTANCE.getFan(),
			control(BowlingPackage.eINSTANCE.getFan_Name()),
			control(BowlingPackage.eINSTANCE.getFan_EMails()),
			control(BowlingPackage.eINSTANCE.getFan_NumberOfTournamentsVisited()),
			control(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise()),
			control(BowlingPackage.eINSTANCE.getFan_FanMerchandise()));

		@SuppressWarnings("restriction")
		final EMFFormsSpreadsheetExporter viewRenderer = new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl(
			new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl.ViewProvider() {
				@Override
				public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
					return view;
				}
			});

		final Workbook wb = viewRenderer.render(Collections.singleton(domainModel), null, null);

		final File targetFile = new File("export.xls"); //$NON-NLS-1$
		saveWorkbook(wb, targetFile.getAbsolutePath());

		// read data
		final FileInputStream file = new FileInputStream(targetFile);
		final Workbook workbook = new HSSFWorkbook(file);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> fans = spreadsheetImport.importSpreadsheet(workbook,
			BowlingPackage.eINSTANCE.getFan()).getImportedEObjects();

		assertEquals(1, fans.size());
		final Fan importedFan = (Fan) fans.iterator().next();
		assertFalse(importedFan.isSetName());
		assertFalse(importedFan.isSetEMails());
		assertTrue(importedFan.isSetNumberOfTournamentsVisited());
		assertEquals(1, importedFan.getNumberOfTournamentsVisited());
		assertFalse(importedFan.isSetFavouriteMerchandise());
		assertFalse(importedFan.isSetFanMerchandise());
		assertTrue(EcoreUtil.equals(domainModel, importedFan));
	}

	@Test
	public void testMixingUnsetFeatures() throws IOException, DatatypeConfigurationException {

		final Fan domainModel1 = BowlingFactory.eINSTANCE.createFan();
		domainModel1.setName("Hans"); //$NON-NLS-1$
		domainModel1.getFanMerchandise().addAll(Arrays.asList(merchandise("1"), merchandise("2"))); //$NON-NLS-1$//$NON-NLS-2$

		final Fan domainModel2 = BowlingFactory.eINSTANCE.createFan();
		domainModel2.getEMails().addAll(Arrays.asList("hans@eclipse.org", "hans@hans.com")); //$NON-NLS-1$//$NON-NLS-2$
		domainModel2.setFavouriteMerchandise(merchandise("fav")); //$NON-NLS-1$

		final VView view = view(BowlingPackage.eINSTANCE.getFan(),
			control(BowlingPackage.eINSTANCE.getFan_Name()),
			control(BowlingPackage.eINSTANCE.getFan_EMails()),
			control(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise()),
			control(BowlingPackage.eINSTANCE.getFan_FanMerchandise()));

		@SuppressWarnings("restriction")
		final EMFFormsSpreadsheetExporter viewRenderer = new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl(
			new org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl.ViewProvider() {
				@Override
				public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
					return view;
				}
			});

		final Workbook wb = viewRenderer.render(Arrays.asList(domainModel1, domainModel2), null, null);

		final File targetFile = new File("export.xls"); //$NON-NLS-1$
		saveWorkbook(wb, targetFile.getAbsolutePath());

		// read data
		final FileInputStream file = new FileInputStream(targetFile);
		final Workbook workbook = new HSSFWorkbook(file);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> fans = spreadsheetImport.importSpreadsheet(workbook,
			BowlingPackage.eINSTANCE.getFan()).getImportedEObjects();

		assertEquals(2, fans.size());
		final Iterator<EObject> iterator = fans.iterator();
		assertTrue(EcoreUtil.equals(domainModel1, iterator.next()));
		assertTrue(EcoreUtil.equals(domainModel2, iterator.next()));
	}

	private static VView view(EClass rootEClass, VContainedElement... elements) {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(rootEClass);
		view.getChildren().addAll(Arrays.asList(elements));
		return view;
	}

	private static VControl control(EStructuralFeature feature) {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(feature);
		return control;
	}

	private static Merchandise merchandise(String name) {
		final Merchandise merchandise = BowlingFactory.eINSTANCE.createMerchandise();
		merchandise.setName(name);
		return merchandise;
	}

}
