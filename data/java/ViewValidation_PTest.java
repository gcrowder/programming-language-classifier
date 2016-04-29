/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.spi.validation.ValidationService;
import org.eclipse.emf.ecp.view.spi.validation.ViewValidationListener;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.Container;
import org.eclipse.emf.ecp.view.validation.test.model.Content;
import org.eclipse.emf.ecp.view.validation.test.model.Library;
import org.eclipse.emf.ecp.view.validation.test.model.Mainboard;
import org.eclipse.emf.ecp.view.validation.test.model.PowerBlock;
import org.eclipse.emf.ecp.view.validation.test.model.Referencer;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author Eugen Neufeld
 * @author emueller
 */

public class ViewValidation_PTest extends CommonValidationTest {

	private DefaultRealm defaultRealm;

	@Before
	public void setup() {
		defaultRealm = new DefaultRealm();
	}

	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	@Test
	public void testValidationMissingElementOnInit() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
			TestPackage.eINSTANCE.getComputer_Mainboard()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);

		assertEquals("Severity must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationMissingElementSetValueAfterInit() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
			TestPackage.eINSTANCE.getComputer_Mainboard()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);
		computer.getMainboard().setName("bla");
		assertEquals("Severity must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	private VDomainModelReference getVTableDomainModelReference(EStructuralFeature feature,
		EReference... eReferences) {
		final VTableDomainModelReference result = VTableFactory.eINSTANCE.createTableDomainModelReference();
		result.setDomainModelEFeature(feature);
		result.getDomainModelEReferencePath().addAll(Arrays.asList(eReferences));
		return result;
	}

	@Test
	public void testValidationInitOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);

		assertEquals("Severity must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationSameFeature() {
		final Mainboard mainboard = TestFactory.eINSTANCE.createMainboard();
		final PowerBlock powerBlock = TestFactory.eINSTANCE.createPowerBlock();
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setMainboard(mainboard);
		computer.setPowerBlock(powerBlock);

		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl powerBlockControl = VViewFactory.eINSTANCE.createControl();
		final VControl mainboardControl = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(powerBlockControl);
		view.getChildren().add(mainboardControl);

		powerBlockControl.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getPowerBlock_Name(),
				TestPackage.eINSTANCE.getComputer_PowerBlock()));

		mainboardControl.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
				TestPackage.eINSTANCE.getComputer_Mainboard()));

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		mainboard.setName("foo");

		assertEquals("Severity must be ok", Diagnostic.OK,
			mainboardControl.getDiagnostic().getHighestSeverity());
		assertEquals("Severity must be error", Diagnostic.ERROR,
			powerBlockControl.getDiagnostic().getHighestSeverity());
		assertEquals("Severity must be error", Diagnostic.ERROR,
			view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitDisabledControlOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setEnabled(false);

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);
		assertNull(control.getDiagnostic());
	}

	@Test
	public void testValidationInitHiddenControlError() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setVisible(false);

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);
		assertNull(control.getDiagnostic());
	}

	@Test
	public void testValidationDisabledControlOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);
		assertEquals("Severity must be Error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
		control.setEnabled(false);
		assertNull(control.getDiagnostic());

	}

	@Test
	public void testValidationHiddenControlOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);
		control.setVisible(false);

		assertEquals("Severity must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitHiddenControlOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);

		assertEquals("Severity must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitError() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);

		assertEquals("Severity must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitAggregation() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final PowerBlock power = TestFactory.eINSTANCE.createPowerBlock();
		power.setName("bla");
		final Mainboard board = TestFactory.eINSTANCE.createMainboard();
		computer.setMainboard(board);
		computer.setPowerBlock(power);

		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl controlPower = VViewFactory.eINSTANCE.createControl();
		controlPower.setDomainModelReference(getVFeaturePathDomainModelReference(
			TestPackage.eINSTANCE.getPowerBlock_Name(), TestPackage.eINSTANCE.getComputer_PowerBlock()));

		final VControl controlBoard = VViewFactory.eINSTANCE.createControl();
		controlBoard.setDomainModelReference(getVFeaturePathDomainModelReference(
			TestPackage.eINSTANCE.getMainboard_Name(), TestPackage.eINSTANCE.getComputer_Mainboard()));

		view.getChildren().add(controlBoard);
		view.getChildren().add(controlPower);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		assertEquals("Severity of mainboard must be error", Diagnostic.ERROR, controlBoard.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of power must be ok", Diagnostic.OK, controlPower.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitPropagation1LevelOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		column.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(column, computer);

		assertEquals("Severity of control must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of column must be ok", Diagnostic.OK, column.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitPropagation1LevelError() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();

		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		column.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(column, computer);

		assertEquals("Severity of control must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be error", Diagnostic.ERROR, column.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitPropagation2LevelOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VView view = VViewFactory.eINSTANCE.createView();
		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		column.getChildren().add(control);
		view.getChildren().add(column);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		assertEquals("Severity of control must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of column must be ok", Diagnostic.OK, column.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationInitPropagation2LevelError() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();

		final VView view = VViewFactory.eINSTANCE.createView();

		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		column.getChildren().add(control);
		view.getChildren().add(column);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		assertEquals("Severity of control must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be error", Diagnostic.ERROR, column.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationChangeOkToError() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);

		computer.setName(null);

		assertEquals("Severity must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationChangeErrorToOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, computer);

		computer.setName("bla");

		assertEquals("Severity must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationPropagationChangeOkToError() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		computer.setName(null);

		assertEquals("Severity of control must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationPropagationChangeErrorTokWithErrorControlInBackground() {

		final Writer writer = TestFactory.eINSTANCE.createWriter();
		writer.setLastName("foo");

		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl firstNameControl = VViewFactory.eINSTANCE.createControl();
		final VControl lastNameControl = VViewFactory.eINSTANCE.createControl();
		firstNameControl.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getWriter_FirstName()));
		lastNameControl.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getWriter_LastName()));
		view.getChildren().add(firstNameControl);
		view.getChildren().add(lastNameControl);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, writer);

		// first name is null -> ERR, last name is 'foo' -> ERR
		assertEquals("Severity of control must be error", Diagnostic.ERROR, firstNameControl.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, lastNameControl.getDiagnostic()
			.getHighestSeverity());

		writer.setLastName("hello!");

		assertEquals("Severity of firstname control must be error", Diagnostic.ERROR, firstNameControl.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of lastname control must be ok", Diagnostic.OK, lastNameControl.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationPropagationChangeErrorTokWithErrorTableControlInBackground() {

		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		writer.setLastName("foo");

		final VView view = VViewFactory.eINSTANCE.createView();
		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		final VTableControl control2 = VTableFactory.eINSTANCE.createTableControl();
		control2
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc2.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_LastName());
		VTableDomainModelReference.class.cast(control2.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc2);

		view.getChildren().add(control);
		view.getChildren().add(control2);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		// first name is null -> ERR, last name is 'foo' -> ERR
		assertEquals("Severity of control must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, control2.getDiagnostic()
			.getHighestSeverity());

		writer.setLastName("hello!");

		assertEquals("Severity of control must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, control2.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationPropagationChangeErrorToOk() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getComputer_Name()));

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		computer.setName("bla");

		assertEquals("Severity of control must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	// TODO fails as the control creates all intermediate domain objects it needs and registers with them, thus the
	// added object is not recognized during validation and the validation is not successful.
	// In order to allow the dynamic behavior on normal controls, each control has to know its domain object directly
	@Test
	public void testValidationDynamicExtension() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");

		final VView view = VViewFactory.eINSTANCE.createView();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
				TestPackage.eINSTANCE.getComputer_Mainboard()));
		view.getChildren().add(control);

		// TODO
		// final Mainboard mainboard = TestFactory.eINSTANCE.createMainboard();
		// computer.setMainboard(mainboard);

		assertEquals("Severity of mainboard name must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationDynamicExtensionWithAlreadyInvalidDomainElement() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");

		final VView view = VViewFactory.eINSTANCE.createView();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
				TestPackage.eINSTANCE.getComputer_Mainboard()));
		final Mainboard mainboard = TestFactory.eINSTANCE.createMainboard();
		computer.setMainboard(mainboard);

		view.getChildren().add(control);

		assertEquals("Severity of mainboard name must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationDynamicRemove() {
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("bla");

		final VView view = VViewFactory.eINSTANCE.createView();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		final VControl control = VViewFactory.eINSTANCE.createControl();

		control.setDomainModelReference(
			getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
				TestPackage.eINSTANCE.getComputer_Mainboard()));
		view.getChildren().add(control);

		// TODO
		// final Mainboard mainboard = TestFactory.eINSTANCE.createMainboard();
		// computer.setMainboard(mainboard);

		assertEquals("Severity of mainboard name must be error", Diagnostic.ERROR, view.getDiagnostic()
			.getHighestSeverity());

		view.getChildren().remove(control);

		assertEquals("Severity of mainboard name must be ok", Diagnostic.OK, view.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationDynamicExtension2Level() {
		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		library.getWriters().add(writer);

		final VView view = VViewFactory.eINSTANCE.createView();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, library);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		column.getChildren().add(control);
		view.getChildren().add(column);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be error", Diagnostic.ERROR, column.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic()
			.getHighestSeverity());

		writer.setFirstName("hello!");

		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be ok", Diagnostic.OK, column.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationTableControl() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");

		final VView view = VViewFactory.eINSTANCE.createView();
		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		view.getChildren().add(control);
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildren() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);

		final VView view = VViewFactory.eINSTANCE.createView();
		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		view.getChildren().add(control);
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenTwoEntries() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		writer2.setFirstName("bla");
		lib.getWriters().add(writer2);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenUpdateExisting() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, lib);

		writer.setFirstName("bla");
		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Ignore
	@Test
	public void testValidationTableControlChildrenUpdateExistingTwoEntries() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer2);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control
			.setDomainModelReference(
				getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, lib);
		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
		writer2.setFirstName("bla");
		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
		writer.setFirstName("bla");
		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenAddNew() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		writer.setFirstName("bla");

		final VView view = VViewFactory.eINSTANCE.createView();

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		view.getChildren().add(control);
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);
		assertEquals("Severity of table must be OK", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());

		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer2);
		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenAddNewWithoutError() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, lib);
		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());

		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		writer2.setFirstName("bla");
		lib.getWriters().add(writer2);
		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenRemoveAdded() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		writer.setFirstName("H");
		lib.getWriters().add(writer);

		final VView view = VViewFactory.eINSTANCE.createView();

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		view.getChildren().add(control);
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be warning", Diagnostic.WARNING, control.getDiagnostic()
			.getHighestSeverity());

		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer2);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());

		lib.getWriters().remove(writer2);

		assertEquals("Severity of table must be warning", Diagnostic.WARNING, control.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenPropagation() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		writer2.setFirstName("bla");
		lib.getWriters().add(writer2);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VView view = VViewFactory.eINSTANCE.createView();
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationChildHasMoreSevereValidationPropagation() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("warning");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final Writer validWriter = TestFactory.eINSTANCE.createWriter();
		validWriter.setFirstName("hello");
		lib.getWriters().add(writer);
		lib.getWriters().add(validWriter);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VView view = VViewFactory.eINSTANCE.createView();
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());

		lib.getWriters().remove(writer);

		// library name does not influence validation result
		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationNotReferencedAttributesAreIgnored() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("warning");
		final Writer validWriter = TestFactory.eINSTANCE.createWriter();
		validWriter.setFirstName("hello");
		lib.getWriters().add(validWriter);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VView view = VViewFactory.eINSTANCE.createView();
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);
		// library name does not influence validation result
		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenPropagationErrorToOk() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		writer2.setFirstName("bla");
		lib.getWriters().add(writer2);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VView view = VViewFactory.eINSTANCE.createView();
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		writer.setFirstName("foo");

		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenPropagationErrorToOkByRemove() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		writer2.setFirstName("bla");
		lib.getWriters().add(writer2);

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VView view = VViewFactory.eINSTANCE.createView();
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		lib.getWriters().remove(writer);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlParentReferencingEntityNotValid() {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.getWriters().add(writer);
		writer.setFirstName("bar");
		writer.setLastName("foo"); // error

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VView view = VViewFactory.eINSTANCE.createView();
		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());

		writer.setFirstName("");
		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());

	}

	@Test
	public void testValidationTableControlAggregation() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		final Writer writer3 = TestFactory.eINSTANCE.createWriter();

		lib.setName("bla");
		lib.getWriters().add(writer);
		lib.getWriters().add(writer2);
		lib.getWriters().add(writer3);
		writer3.setFirstName("bla");

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setName("firstNameTable");
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		final VTableControl control2 = VTableFactory.eINSTANCE.createTableControl();
		control2.setName("lastNameTable");
		control2.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc2.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_LastName());
		VTableDomainModelReference.class.cast(control2.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc2);

		view.getChildren().add(control1);
		view.getChildren().add(control2);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of table must be ok", Diagnostic.OK, control2.getDiagnostic().getHighestSeverity());

		lib.getWriters().remove(writer);
		lib.getWriters().remove(writer2);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsInitOk() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("b");
		container.getContents().add(content2);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic().getHighestSeverity());
		assertEquals("There must be 0 diagnostics", 0, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsInitError() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("a");
		container.getContents().add(content2);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 2x error for contents
		// assertEquals("There must be 3 diagnostics", 3, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsDynamicFirstOkToError() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("b");
		container.getContents().add(content2);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic().getHighestSeverity());
		assertEquals("There must be 0 diagnostics", 0, control1.getDiagnostic().getDiagnostics().size());

		content1.setUniqueAttribute("b");

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsDynamicSecondOkToError() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("b");
		container.getContents().add(content2);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic().getHighestSeverity());
		assertEquals("There must be 0 diagnostics", 0, control1.getDiagnostic().getDiagnostics().size());

		content2.setUniqueAttribute("a");

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsDynamicFirstErrorToOk() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("a");
		container.getContents().add(content2);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 2x error for contents
		// assertEquals("There must be 3 diagnostics", 3, control1.getDiagnostic().getDiagnostics().size());

		content1.setUniqueAttribute("b");

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic().getHighestSeverity());
		assertEquals("There must be 0 diagnostics", 0, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsDynamicSecondErrorToOk() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("a");
		container.getContents().add(content2);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 2x error for contents
		// assertEquals("There must be 3 diagnostics", 3, control1.getDiagnostic().getDiagnostics().size());

		content2.setUniqueAttribute("b");

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic().getHighestSeverity());
		assertEquals("There must be 0 diagnostics", 0, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsMultipleDuplicatesInitError() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("a");
		container.getContents().add(content2);

		final Content content3 = TestFactory.eINSTANCE.createContent();
		content3.setUniqueAttribute("b");
		container.getContents().add(content3);
		final Content content4 = TestFactory.eINSTANCE.createContent();
		content4.setUniqueAttribute("b");
		container.getContents().add(content4);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 4x error for contents
		// assertEquals("There must be 5 diagnostics", 5, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsMultipleDuplicatesDynamic4ErrorTo3Error() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("a");
		container.getContents().add(content2);

		final Content content3 = TestFactory.eINSTANCE.createContent();
		content3.setUniqueAttribute("b");
		container.getContents().add(content3);
		final Content content4 = TestFactory.eINSTANCE.createContent();
		content4.setUniqueAttribute("b");
		container.getContents().add(content4);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 4x error for contents
		// assertEquals("There must be 5 diagnostics", 5, control1.getDiagnostic().getDiagnostics().size());

		content3.setUniqueAttribute("a");

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 1x ok , 3x error for contents
		// assertEquals("There must be 5 diagnostics", 5, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlMultipleDiagnosticsMultipleDuplicatesDynamic4ErrorTo2Error() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Container container = TestFactory.eINSTANCE.createContainer();
		final Content content1 = TestFactory.eINSTANCE.createContent();
		content1.setUniqueAttribute("a");
		container.getContents().add(content1);
		final Content content2 = TestFactory.eINSTANCE.createContent();
		content2.setUniqueAttribute("a");
		container.getContents().add(content2);

		final Content content3 = TestFactory.eINSTANCE.createContent();
		content3.setUniqueAttribute("b");
		container.getContents().add(content3);
		final Content content4 = TestFactory.eINSTANCE.createContent();
		content4.setUniqueAttribute("b");
		container.getContents().add(content4);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getContainer_Contents()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getContent_UniqueAttribute());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, container);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container, 4x error for contents
		// assertEquals("There must be 5 diagnostics", 5, control1.getDiagnostic().getDiagnostics().size());

		content3.setUniqueAttribute("c");

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		// ok for container,2x ok, 2x error for contents
		// assertEquals("There must be 4 diagnostics", 4, control1.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testValidationTableControlTwoColumns() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();

		lib.setName("bla");
		lib.getWriters().add(writer);

		final VTableControl control1 = VTableFactory.eINSTANCE.createTableControl();
		control1.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc1.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		final VFeaturePathDomainModelReference tc2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc2.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_LastName());
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc1);
		VTableDomainModelReference.class.cast(control1.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc2);

		view.getChildren().add(control1);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR,
			control1.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of table must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());

		writer.setFirstName("asdf");

		assertEquals("Severity of table must be ok", Diagnostic.OK, control1.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of table must be ok", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationTableControlChildrenRemoveEntry() {
		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("bla");
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer);

		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer2);
		final Writer writer3 = TestFactory.eINSTANCE.createWriter();
		lib.getWriters().add(writer3);
		writer3.setFirstName("bla");

		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();
		control
			.setDomainModelReference(
				getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);

		ViewModelContextFactory.INSTANCE.createViewModelContext(control, lib);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());

		lib.getWriters().remove(writer);

		assertEquals("Severity of table must be error", Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());

		lib.getWriters().remove(writer2);

		assertEquals("Severity of table must be ok", Diagnostic.OK, control.getDiagnostic()
			.getHighestSeverity());
	}

	@Test
	public void testValidationNonDomainWithDomainLoseChild() {
		// domain
		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();

		// non domain
		final VView view = VViewFactory.eINSTANCE.createView();
		final VTableControl tableControl = VTableFactory.eINSTANCE.createTableControl();
		tableControl.setDomainModelReference(
			getVTableDomainModelReference(TestPackage.eINSTANCE.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(tableControl.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();

		column.getChildren().add(tableControl);
		view.getChildren().add(column);

		library.getWriters().add(writer);
		writer.setFirstName("aaa");
		library.getWriters().add(writer2);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, library);

		assertEquals("Severity of control must be error", Diagnostic.ERROR, tableControl.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be error", Diagnostic.ERROR, column.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());

		library.getWriters().remove(writer2);

		assertEquals("Severity of control must be ok", Diagnostic.OK,
			tableControl.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of column must be OK", Diagnostic.OK, column.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be OK", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationPropagation2LevelErrorToOkDomainObjectChanges() {
		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		library.getWriters().add(writer);

		final VView view = VViewFactory.eINSTANCE.createView();

		final VTableControl tableControl = VTableFactory.eINSTANCE.createTableControl();
		tableControl.setDomainModelReference(getVTableDomainModelReference(TestPackage.eINSTANCE
			.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(tableControl.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();

		column.getChildren().add(tableControl);
		view.getChildren().add(column);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, library);

		assertEquals("Severity of control must be error", Diagnostic.ERROR, tableControl.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be error", Diagnostic.ERROR, column.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be error", Diagnostic.ERROR, view.getDiagnostic().getHighestSeverity());

		writer.setFirstName("hello");

		assertEquals("Severity of control must be ok", Diagnostic.OK, tableControl.getDiagnostic()
			.getHighestSeverity());
		assertEquals("Severity of column must be OK", Diagnostic.OK, column.getDiagnostic().getHighestSeverity());
		assertEquals("Severity of view must be OK", Diagnostic.OK, view.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationDynamicAddManyToDomain() {
		// setup
		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		writer.setFirstName("Name");
		library.getWriters().add(writer);

		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TestPackage.eINSTANCE.getLibrary());

		final VTableControl tableControl = VTableFactory.eINSTANCE.createTableControl();
		tableControl.setDomainModelReference(getVTableDomainModelReference(TestPackage.eINSTANCE
			.getLibrary_Writers()));
		final VFeaturePathDomainModelReference tc = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		tc.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		VTableDomainModelReference.class.cast(tableControl.getDomainModelReference()).getColumnDomainModelReferences()
			.add(tc);
		view.getChildren().add(tableControl);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, library);

		assertEquals("Severity of control must be ok", Diagnostic.OK, tableControl.getDiagnostic()
			.getHighestSeverity());

		// act
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		final Writer writer3 = TestFactory.eINSTANCE.createWriter();
		writer3.setFirstName("H");
		final List<Writer> writers = new ArrayList<Writer>();
		writers.add(writer2);
		writers.add(writer3);
		library.getWriters().addAll(writers);

		// assert
		assertEquals("Severity of control must be Error", Diagnostic.ERROR, tableControl.getDiagnostic()
			.getHighestSeverity());

	}

	@Test
	public void testNonContainmentReferenceInitialValidationOK() {
		/* setup model */
		final Referencer referencer = TestFactory.eINSTANCE.createReferencer();
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		referencer.setReferencedContent(computer);
		computer.setName("Raspberry Pi");

		/* setup view */
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		control.setDomainModelReference(TestPackage.eINSTANCE.getComputer_Name(),
			Collections.singleton(TestPackage.eINSTANCE.getReferencer_ReferencedContent()));

		/* act */
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, referencer);

		/* assert */
		assertEquals("Severity of control must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testNonContainmentReferenceInitialValidationError() {
		/* setup model */
		final Referencer referencer = TestFactory.eINSTANCE.createReferencer();
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		referencer.setReferencedContent(computer);
		computer.setName(null);

		/* setup view */
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		control.setDomainModelReference(TestPackage.eINSTANCE.getComputer_Name(),
			Collections.singleton(TestPackage.eINSTANCE.getReferencer_ReferencedContent()));

		/* act */
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, referencer);

		/* assert */
		assertEquals("Severity of control must be error", Diagnostic.ERROR,
			control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testNonContainmentReferenceValidationOnChangeOK() {
		/* setup model */
		final Referencer referencer = TestFactory.eINSTANCE.createReferencer();
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		referencer.setReferencedContent(computer);
		computer.setName(null);

		/* setup view */
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		control.setDomainModelReference(TestPackage.eINSTANCE.getComputer_Name(),
			Collections.singleton(TestPackage.eINSTANCE.getReferencer_ReferencedContent()));

		/* setup validation service */
		final ViewModelContext context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, referencer);

		/* act */
		computer.setName("Raspberry Pi");
		/*
		 * auto revalidate does not work with our model, since we have a non-containment and made no adjustments to the
		 * model impl. this _is_ expected
		 */
		context.getService(ValidationService.class).validate(Collections.<EObject> singleton(computer));

		/* assert */
		assertEquals("Severity of control must be ok", Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testNonContainmentReferenceValidationOnChangeError() {
		/* setup model */
		final Referencer referencer = TestFactory.eINSTANCE.createReferencer();
		final Computer computer = TestFactory.eINSTANCE.createComputer();
		referencer.setReferencedContent(computer);
		computer.setName("Raspberry Pi");

		/* setup view */
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		control.setDomainModelReference(TestPackage.eINSTANCE.getComputer_Name(),
			Collections.singleton(TestPackage.eINSTANCE.getReferencer_ReferencedContent()));

		/* setup validation service */
		final ViewModelContext context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, referencer);

		/* act */
		computer.setName(null);
		/*
		 * auto revalidate does not work with our model, since we have a non-containment and made no adjustments to the
		 * model impl. this _is_ expected
		 */
		context.getService(ValidationService.class).validate(Collections.<EObject> singleton(computer));

		/* assert */
		assertEquals("Severity of control must be error", Diagnostic.ERROR,
			control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testRegisterListener() {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control
			.setDomainModelReference(
				getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getWriter_FirstName()));
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(control, writer);

		final Set<Diagnostic> result = new LinkedHashSet<Diagnostic>();

		final ViewValidationListener listener = new ViewValidationListener() {
			@Override
			public void onNewValidation(Set<Diagnostic> validationResults) {
				result.addAll(validationResults);
			}
		};

		final ValidationService service = vmc.getService(ValidationService.class);
		service.registerValidationListener(listener);

		assertEquals("One Diagnostic expected", 1, result.size());
		assertEquals("Severity of control must be Error", Diagnostic.ERROR, result.iterator().next().getSeverity());
	}

	@Test
	public void testListenerUponChange() throws InterruptedException {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control
			.setDomainModelReference(
				getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getWriter_FirstName()));

		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(control, writer);

		final List<CountDownLatch> latch = new ArrayList<CountDownLatch>();
		latch.add(new CountDownLatch(1));
		final Set<Diagnostic> lastResult = new LinkedHashSet<Diagnostic>();

		final ViewValidationListener listener = new ViewValidationListener() {
			@Override
			public void onNewValidation(Set<Diagnostic> validationResults) {
				lastResult.clear();
				lastResult.addAll(validationResults);
				latch.get(0).countDown();
			}
		};

		final ValidationService service = vmc.getService(ValidationService.class);
		service.registerValidationListener(listener);

		latch.get(0).await(1, TimeUnit.SECONDS);
		assertEquals("One Diagnostic expected", 1, lastResult.size());
		latch.clear();
		latch.add(new CountDownLatch(1));

		writer.setFirstName("Hans");
		latch.get(0).await(1, TimeUnit.SECONDS);
		assertEquals("No Diagnostic expected since OK", 0, lastResult.size());
		latch.clear();
		latch.add(new CountDownLatch(1));

		writer.setFirstName("");
		latch.get(0).await(1, TimeUnit.SECONDS);
		assertEquals("One Diagnostic expected", 1, lastResult.size());
		assertEquals("Severity of control must be Error", Diagnostic.ERROR, lastResult.iterator().next().getSeverity());
	}
}
