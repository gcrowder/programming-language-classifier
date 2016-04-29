/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.ecp.view.test.common.spi.GCCollectable;
import org.eclipse.emf.ecp.view.test.common.spi.Tuple;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Tests for checking whether the {@link org.eclipse.emf.ecp.view.spi.validation.ValidationService
 * ValidationService} correctly behave if domain
 * objects and/or {@link org.eclipse.emf.ecp.view.spi.model.VElement Renderable}s are removed.
 *
 * @author emueller
 */
public class ValidationServiceGC_PTest extends CommonValidationTest {

	private DefaultRealm defaultRealm;

	@Before
	public void setup() {
		defaultRealm = new DefaultRealm();
	}

	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	/**
	 * Creates a basic view with an column that contains a control
	 * that is bound to the mainboard name feature of a computer.
	 * Both the computer and the mainboard are also created.
	 *
	 * @return a tuple containing the root of the view model
	 *         as well as the root of the domain model
	 */
	protected Tuple<VView, Computer> createComputerView() {

		final Computer computer = TestFactory.eINSTANCE.createComputer();
		final VView view = VViewFactory.eINSTANCE.createView();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);

		final VControl control = VViewFactory.eINSTANCE.createControl();
		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(column);

		control.setDomainModelReference(getVFeaturePathDomainModelReference(TestPackage.eINSTANCE.getMainboard_Name(),
			TestPackage.eINSTANCE.getComputer_Mainboard()));
		column.getChildren().add(control);

		// TODO
		// final Mainboard mainboard = TestFactory.eINSTANCE.createMainboard();
		// computer.setMainboard(mainboard);

		assertEquals("Severity of mainboard name must be error", Diagnostic.ERROR, control.getDiagnostic()
			.getHighestSeverity());

		return new Tuple<VView, Computer>(view, computer);
	}

	/**
	 * Creates a basic view with an two nested columns.
	 * The most inner column contains a control
	 * that is bound to the {@link TestPackage#getWriter_FirstName()} feature of a {@link Writer}.
	 * The writer will also be created by this method.
	 *
	 * @return a tuple containing the root of the view model
	 *         as well as the root of the domain model
	 */
	protected Tuple<VView, Writer> createWriterWithNestedColumnsView() {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(writer.eClass());

		final VVerticalLayout parentColumn = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(parentColumn);

		final VVerticalLayout column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		parentColumn.getChildren().add(column);

		final VControl controlWriter = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(TestPackage.eINSTANCE.getWriter_FirstName());
		controlWriter.setDomainModelReference(domainModelReference);

		column.getChildren().add(controlWriter);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, writer);

		return new Tuple<VView, Writer>(view, writer);
	}

	/**
	 * Removes the direct column of the view. The nested column as
	 * well as the control should be removed and thus not referenced
	 * anymore.
	 */
	@Test
	public void testRemoveRenderableHierarchy() {

		final VView view = createWriterWithNestedColumnsView().first();
		final GCCollectable parentColumnCollectable = new GCCollectable(
			view.getChildren().get(0));
		final GCCollectable columnCollectable = new GCCollectable(
			VVerticalLayout.class.cast(view.getChildren().get(0)).getChildren().get(0));
		final GCCollectable controlCollectable = new GCCollectable(
			VVerticalLayout.class.cast(
				VVerticalLayout.class.cast(view.getChildren().get(0))
					.getChildren().get(0))
				.getChildren()
				.get(0));

		view.getChildren().remove(0);

		assertTrue(parentColumnCollectable.isCollectable());
		assertTrue(columnCollectable.isCollectable());
		assertTrue(controlCollectable.isCollectable());
	}

	/**
	 * Removes the direct column of the view. The nested column as
	 * well as the control should be removed and thus not referenced
	 * anymore.
	 */
	@Test
	public void testRemoveControlAndReevaluate() {

		final VView view = createWriterWithNestedColumnsView().first();

		final GCCollectable controlCollectable = new GCCollectable(
			VVerticalLayout.class.cast(VVerticalLayout.class.cast(
				view.getChildren().get(0)).getChildren().get(0)).getChildren().get(0));

		assertEquals(Diagnostic.ERROR,
			VVerticalLayout.class.cast(view.getChildren().get(0)).getDiagnostic().getHighestSeverity());

		VVerticalLayout.class.cast(VVerticalLayout.class.cast(
			view.getChildren().get(0)).getChildren().get(0)).getChildren().remove(0);

		assertEquals(Diagnostic.OK,
			VVerticalLayout.class.cast(view.getChildren().get(0)).getDiagnostic().getHighestSeverity());

		assertTrue(controlCollectable.isCollectable());
	}

	/**
	 * Make sure child domain object is actually referenced.
	 */
	@Test
	public void testDomainObjectIsReferenced() {

		final Tuple<VView, Computer> t = createComputerView();

		final GCCollectable mainboardCollectable = new GCCollectable(
			t.second().getMainboard());

		assertFalse(mainboardCollectable.isCollectable());
	}

	/**
	 * Remove the {@link org.eclipse.emf.ecp.view.validation.test.model.Mainboard Mainboard}. It shouldn't be referenced
	 * anymore
	 * in contrast to the control.
	 */
	@Ignore
	@Test
	public void testRemoveChildOfDomainObject() {

		final Tuple<VView, Computer> t = createComputerView();

		final GCCollectable mainboardCollectable = new GCCollectable(
			t.second().getMainboard());
		final GCCollectable controlCollectable = new GCCollectable(
			VVerticalLayout.class.cast(t.first().getChildren().get(0)).getChildren().get(0));
		t.second().setMainboard(null);

		// control for mainboard should be removed from
		assertTrue(mainboardCollectable.isCollectable());
		assertFalse(controlCollectable.isCollectable());
	}

	/**
	 * Removes the {@link org.eclipse.emf.ecp.view.validation.test.model.Mainboard Mainboard} from the computer and also
	 * removes the control
	 * from the view model containment tree.
	 * Neither the mainboard nor the control should be referenced afterwards.
	 */
	@Ignore
	@Test
	public void testRemoveChildOfDomainObjectWithCutOffControl() {

		final Tuple<VView, Computer> t = createComputerView();

		final GCCollectable mainboardCollectable = new GCCollectable(
			t.second().getMainboard());
		final GCCollectable controlCollectable = new GCCollectable(
			VVerticalLayout.class.cast(t.first().getChildren().get(0)).getChildren().get(0));

		t.second().setMainboard(null);

		VVerticalLayout.class.cast(t.first().getChildren().get(0)).getChildren().clear();

		// control for mainboard shouldn't be referenced anymore by validation registry or service
		assertTrue(mainboardCollectable.isCollectable());
		assertTrue(controlCollectable.isCollectable());
	}

	/**
	 * Removes the direct column of the computer view.
	 */
	@Test
	public void testRemoveRenderable() {

		final VView view = createComputerView().first();
		final GCCollectable collectable = new GCCollectable(view.getChildren().get(0));
		// removes the column
		view.getChildren().remove(0);

		assertTrue(collectable.isCollectable());
	}
}
