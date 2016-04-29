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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeConstants;

import org.apache.poi.ss.usermodel.Workbook;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
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
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewModelProperties;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl;
import org.eclipse.emfforms.internal.spreadsheet.core.transfer.EMFFormsSpreadsheetExporterImpl.ViewProvider;
import org.eclipse.emfforms.spi.spreadsheet.core.error.model.SpreadsheetImportResult;
import org.eclipse.emfforms.spi.spreadsheet.core.transfer.EMFFormsSpreadsheetExporter;
import org.eclipse.emfforms.spi.spreadsheet.core.transfer.EMFFormsSpreadsheetImporter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class EMFFormsSpreadsheetImportImpl_ITest {

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
	public void testImportSpreadsheet() throws IOException, DatatypeConfigurationException {
		final EMFFormsSpreadsheetExporter viewRenderer = new EMFFormsSpreadsheetExporterImpl(new ViewProvider() {
			@Override
			public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
				return getView();
			}
		});
		final User user = getDomainModel();
		final User user2 = getDomainModel();

		final Workbook workbook = viewRenderer.render(Arrays.asList(user, user2), null, null);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final SpreadsheetImportResult importResult = spreadsheetImport.importSpreadsheet(workbook,
			TaskPackage.eINSTANCE.getUser());
		final Collection<EObject> users = importResult.getImportedEObjects();
		final Iterator<EObject> iterator = users.iterator();
		assertTrue(EcoreUtil.equals(user, iterator.next()));
		assertTrue(EcoreUtil.equals(user2, iterator.next()));
	}

	private User getDomainModel() throws DatatypeConfigurationException {
		final User user = TaskFactory.eINSTANCE.createUser();
		user.setEmail("myEMail@test.de"); //$NON-NLS-1$
		user.setFirstName("Bob"); //$NON-NLS-1$
		user.setHeigth(2);
		user.setLastName("Smith"); //$NON-NLS-1$
		user.setNationality(Nationality.US);
		user.setTimeOfRegistration(new Date());
		user.setWeight(1.45);
		final XMLCalendar cal = new XMLCalendar(new Date(), XMLCalendar.DATE);
		cal.setTimezone(DatatypeConstants.FIELD_UNDEFINED);
		cal.setHour(DatatypeConstants.FIELD_UNDEFINED);
		cal.setMinute(DatatypeConstants.FIELD_UNDEFINED);
		cal.setSecond(DatatypeConstants.FIELD_UNDEFINED);
		cal.setMillisecond(DatatypeConstants.FIELD_UNDEFINED);
		user.setDateOfBirth(cal);
		return user;
	}

	private VView getView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TaskPackage.eINSTANCE.getUser());
		final EList<EStructuralFeature> structuralFeatures = TaskPackage.eINSTANCE.getUser()
			.getEAllStructuralFeatures();
		for (final EStructuralFeature feature : structuralFeatures) {
			if (EReference.class.isInstance(feature)) {
				continue;
			}
			final VControl control = VViewFactory.eINSTANCE.createControl();
			final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			modelReference.setDomainModelEFeature(feature);
			control.setDomainModelReference(modelReference);
			view.getChildren().add(control);
		}
		return view;
	}

	@Test
	public void testImportSpreadsheetUnsetFeatures() throws IOException, DatatypeConfigurationException {
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

		final EMFFormsSpreadsheetExporter viewRenderer = new EMFFormsSpreadsheetExporterImpl(new ViewProvider() {
			@Override
			public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
				return view;
			}
		});

		final Workbook workbook = viewRenderer.render(Collections.singleton(domainModel), null, null);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> fans = spreadsheetImport.importSpreadsheet(workbook,
			BowlingPackage.eINSTANCE.getFan()).getImportedEObjects();

		assertEquals(1, fans.size());
		final Fan importedFan = (Fan) fans.iterator().next();
		assertFalse(importedFan.isSetName());
		assertFalse(importedFan.isSetEMails());
		assertTrue(importedFan.isSetNumberOfTournamentsVisited());
		assertFalse(importedFan.isSetFavouriteMerchandise());
		assertFalse(importedFan.isSetFanMerchandise());
		assertTrue(EcoreUtil.equals(domainModel, importedFan));
	}

	@Test
	public void testImportSpreadsheetUnsettableFeaturesWithDefaultValue()
		throws IOException, DatatypeConfigurationException {

		final Fan domainModel = BowlingFactory.eINSTANCE.createFan();
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_Name(), null);
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_EMails(), Collections.emptyList());
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise(), null);
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_FanMerchandise(), Collections.emptyList());
		assertTrue(domainModel.isSetName());
		assertTrue(domainModel.isSetEMails());
		assertTrue(domainModel.isSetFavouriteMerchandise());
		assertTrue(domainModel.isSetFanMerchandise());

		final VView view = view(BowlingPackage.eINSTANCE.getFan(),
			control(BowlingPackage.eINSTANCE.getFan_Name()),
			control(BowlingPackage.eINSTANCE.getFan_EMails()),
			control(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise()),
			control(BowlingPackage.eINSTANCE.getFan_FanMerchandise()));

		final EMFFormsSpreadsheetExporter viewRenderer = new EMFFormsSpreadsheetExporterImpl(new ViewProvider() {
			@Override
			public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
				return view;
			}
		});

		final Workbook workbook = viewRenderer.render(Collections.singleton(domainModel), null, null);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> fans = spreadsheetImport.importSpreadsheet(workbook,
			BowlingPackage.eINSTANCE.getFan()).getImportedEObjects();

		assertEquals(1, fans.size());
		final Fan importedFan = (Fan) fans.iterator().next();
		assertFalse(importedFan.isSetName());
		assertTrue(importedFan.isSetEMails());
		assertFalse(importedFan.isSetFavouriteMerchandise());
		// TODO the following assertions are failing because of a bug in EMF
		// EMF is using an identity command instead of an empty set command, which would make the feature set
		// assertTrue(importedFan.isSetFanMerchandise());
		// assertTrue(EcoreUtil.equals(domainModel, importedFan));
	}

	@Test
	public void testImportSpreadsheetUnsettableFeaturesWithNonDefaultValue()
		throws IOException, DatatypeConfigurationException {

		final Fan domainModel = BowlingFactory.eINSTANCE.createFan();
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_Name(), "Hans"); //$NON-NLS-1$
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_EMails(), Arrays.asList("hans@eclipse.org", "hans@hans.com")); //$NON-NLS-1$ //$NON-NLS-2$
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise(), merchandise("Favourite")); //$NON-NLS-1$
		domainModel.eSet(BowlingPackage.eINSTANCE.getFan_FanMerchandise(),
			Arrays.asList(merchandise("merc1"), merchandise("merc2"))); //$NON-NLS-1$ //$NON-NLS-2$
		assertTrue(domainModel.isSetName());
		assertTrue(domainModel.isSetEMails());
		assertTrue(domainModel.isSetFavouriteMerchandise());
		assertTrue(domainModel.isSetFanMerchandise());

		final VView view = view(BowlingPackage.eINSTANCE.getFan(),
			control(BowlingPackage.eINSTANCE.getFan_Name()),
			control(BowlingPackage.eINSTANCE.getFan_EMails()),
			control(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise()),
			control(BowlingPackage.eINSTANCE.getFan_FanMerchandise()));

		final EMFFormsSpreadsheetExporter viewRenderer = new EMFFormsSpreadsheetExporterImpl(new ViewProvider() {
			@Override
			public VView getViewModel(EObject viewEobject, VViewModelProperties properties) {
				return view;
			}
		});

		final Workbook workbook = viewRenderer.render(Collections.singleton(domainModel), null, null);

		final EMFFormsSpreadsheetImporter spreadsheetImport = EMFFormsSpreadsheetImporter.INSTANCE;
		final Collection<EObject> fans = spreadsheetImport.importSpreadsheet(workbook,
			BowlingPackage.eINSTANCE.getFan()).getImportedEObjects();

		assertEquals(1, fans.size());
		final Fan importedFan = (Fan) fans.iterator().next();
		assertTrue(importedFan.isSetName());
		assertTrue(importedFan.isSetEMails());
		assertTrue(importedFan.isSetFavouriteMerchandise());
		assertTrue(importedFan.isSetFanMerchandise());
		assertTrue(EcoreUtil.equals(domainModel, importedFan));
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
