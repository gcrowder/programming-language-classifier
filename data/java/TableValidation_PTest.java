/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.table.model.DetailEditing;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.validation.test.model.Library;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithInnerChild2;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithValidation;
import org.eclipse.emf.ecp.view.validation.test.model.TableContentWithoutValidation;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithMultiplicity;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithUnique;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithoutMultiplicity;
import org.eclipse.emf.ecp.view.validation.test.model.TableWithoutMultiplicityConcrete;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * This class contains only tests for validation errors on Tables. This test also doesn't check whether the table
 * updates correctly on diagnostic change. It is only checked, that the Diagnostic is correctly set.
 *
 * Tests:
 * - test table with only reference multiplicity (init empty/not empty, dynamic add/ remove)
 * - test table without reference multiplicity, but with independent validation on children
 *
 * @author Eugen Neufeld
 *
 */
public class TableValidation_PTest {

	private DefaultRealm defaultRealm;

	@Before
	public void setup() {
		defaultRealm = new DefaultRealm();
	}

	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	private VView createViewWithTableControl(EClass rootClass, EReference tableReference,
		EAttribute... columnAttributes) {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(rootClass);
		final VTableControl tableControl = VTableFactory.eINSTANCE.createTableControl();
		view.getChildren().add(tableControl);
		final VTableDomainModelReference domainModelReference = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		tableControl.setDomainModelReference(domainModelReference);
		domainModelReference.setDomainModelEFeature(tableReference);

		for (final EAttribute attribute : columnAttributes) {
			final VFeaturePathDomainModelReference column = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			column.setDomainModelEFeature(attribute);

			VTableDomainModelReference.class.cast(tableControl.getDomainModelReference())
				.getColumnDomainModelReferences().add(column);
		}
		return view;
	}

	@Test
	public void testReferenceMultiplicityInitEmpty() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithMultiplicity(),
			TestPackage.eINSTANCE.getTableWithMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());
		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithMultiplicity domain = TestFactory.eINSTANCE.createTableWithMultiplicity();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		assertEquals(Diagnostic.ERROR, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testReferenceMultiplicityInitNotEmpty() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithMultiplicity(),
			TestPackage.eINSTANCE.getTableWithMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());
		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithMultiplicity domain = TestFactory.eINSTANCE.createTableWithMultiplicity();
		domain.getContent().add(TestFactory.eINSTANCE.createTableContentWithoutValidation());
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testReferenceMultiplicityDynamicEmptyToNotEmpty() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithMultiplicity(),
			TestPackage.eINSTANCE.getTableWithMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());
		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithMultiplicity domain = TestFactory.eINSTANCE.createTableWithMultiplicity();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		domain.getContent().add(TestFactory.eINSTANCE.createTableContentWithoutValidation());

		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());

	}

	@Test
	public void testReferenceMultiplicityDynamicNotEmptyToEmpty() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithMultiplicity(),
			TestPackage.eINSTANCE.getTableWithMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());
		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithMultiplicity domain = TestFactory.eINSTANCE.createTableWithMultiplicity();
		domain.getContent().add(TestFactory.eINSTANCE.createTableContentWithoutValidation());
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		domain.getContent().clear();
		assertEquals(Diagnostic.ERROR, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testError0ChildInit() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testError1ChildInit() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		assertEquals(Diagnostic.ERROR, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testError1ChildDynamicDelete() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		domain.getContent().clear();
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testError1ChildDynamicSetValue() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content.setName("test");
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void test1Error1OkChildInit() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		content2.setName("test");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		assertEquals(Diagnostic.ERROR, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void test1Ok1ErrorChildInit() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		content1.setName("test");
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		assertEquals(Diagnostic.ERROR, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void test1Error1OkChildDynamicDeleteError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		content2.setName("test");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		domain.getContent().remove(0);
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void test1Ok1ErrorChildDynamicDeleteError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		content1.setName("test");
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		domain.getContent().remove(1);
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void test1Error1OkChildDynamicSetValueToError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		content2.setName("test");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content1.setName("test");
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void test1Ok1ErrorChildDynamicSetValueToError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithoutMultiplicity(),
			TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithoutMultiplicity domain = TestFactory.eINSTANCE.createTableWithoutMultiplicity();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		content1.setName("test");
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content2.setName("test");
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueInitNoError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("b");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueInitError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("a");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicAddToError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("a");
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicRemoveToOk() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("a");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		domain.getContent().remove(content1);
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicChange1ToError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("b");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content1.setName("b");
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicChange2ToError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("b");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content2.setName("a");
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicChange1ToOK() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("a");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content1.setName("b");
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicChange2ToOK() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("a");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content2.setName("b");
		assertEquals(Diagnostic.OK, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueDynamicChange3Elements1ToError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithoutValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithoutValidation content1 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithoutValidation content2 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content2);
		content2.setName("b");
		final TableContentWithoutValidation content3 = TestFactory.eINSTANCE.createTableContentWithoutValidation();
		domain.getContent().add(content3);
		content3.setName("b");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
		content2.setName("a");
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueWithContentValidationInitError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		assertEquals(Diagnostic.ERROR, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueWithContentValidationDynamicErrorToWarning() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		content2.setName("a");
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testUniqueWithContentValidationDynamicAddError() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getTableWithUnique(),
			TestPackage.eINSTANCE.getTableWithUnique_Content(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Name(),
			TestPackage.eINSTANCE.getTableContentWithValidation_Weight());

		final VTableControl table = (VTableControl) view.getChildren().get(0);
		final TableWithUnique domain = TestFactory.eINSTANCE.createTableWithUnique();
		final TableContentWithValidation content1 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content1);
		content1.setName("a");
		final TableContentWithValidation content2 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content2);
		content2.setName("b");
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		final TableContentWithValidation content3 = TestFactory.eINSTANCE.createTableContentWithValidation();
		domain.getContent().add(content3);
		content3.setName("b");
		assertEquals(Diagnostic.WARNING, table.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testChangeOfSingleContentElement() {
		final VView view = createViewWithTableControl(TestPackage.eINSTANCE.getLibrary(),
			TestPackage.eINSTANCE.getLibrary_Writers(), TestPackage.eINSTANCE.getWriter_FirstName());
		final VTableControl table = (VTableControl) view.getChildren().get(0);

		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Writer writer1 = TestFactory.eINSTANCE.createWriter();
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		library.getWriters().add(writer1);
		library.getWriters().add(writer2);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, library);

		assertEquals(2, table.getDiagnostic().getDiagnostics().size());

		writer1.setFirstName("test");

		assertEquals(1, table.getDiagnostic().getDiagnostics().size());
	}

	@Test
	public void testTableValidationWithIndirectChildren() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TestPackage.eINSTANCE.getTableWithoutMultiplicity());
		final VTableControl tableControl = VTableFactory.eINSTANCE.createTableControl();
		view.getChildren().add(tableControl);
		final VTableDomainModelReference domainModelReference = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		tableControl.setDomainModelReference(domainModelReference);
		final VFeaturePathDomainModelReference tableDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		tableDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getTableWithoutMultiplicity_Content());
		domainModelReference.setDomainModelReference(tableDMR);
		final VFeaturePathDomainModelReference column = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();

		domainModelReference.getColumnDomainModelReferences().add(column);
		column.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getTableContentWithInnerChild_InnerChild());
		column.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getTableContentWithInnerChild2_InnerChild());
		column.setDomainModelEFeature(TestPackage.eINSTANCE.getTableContentWithValidation_Name());

		final TableWithoutMultiplicity tableWithoutMultiplicity = TestFactory.eINSTANCE
			.createTableWithoutMultiplicity();
		final TableContentWithInnerChild child = TestFactory.eINSTANCE.createTableContentWithInnerChild();
		tableWithoutMultiplicity.getContent().add(child);
		final TableContentWithInnerChild2 innerChild = TestFactory.eINSTANCE.createTableContentWithInnerChild2();
		child.setInnerChild(innerChild);
		final TableContentWithValidation innerInnerChild = TestFactory.eINSTANCE.createTableContentWithValidation();
		innerChild.setInnerChild(innerInnerChild);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, tableWithoutMultiplicity);

		assertEquals(1, tableControl.getDiagnostic().getDiagnostics().size());
		assertEquals(Diagnostic.ERROR, tableControl.getDiagnostic().getHighestSeverity());
		innerInnerChild.setName("a");
		assertEquals(0, tableControl.getDiagnostic().getDiagnostics().size());
		assertEquals(Diagnostic.OK, tableControl.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testTableWithDetailValidationOnDetail() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TestPackage.eINSTANCE.getTableWithoutMultiplicityConcrete());
		final VTableControl tableControl = VTableFactory.eINSTANCE.createTableControl();
		tableControl.setDetailEditing(DetailEditing.WITH_PANEL);
		view.getChildren().add(tableControl);
		final VTableDomainModelReference domainModelReference = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		tableControl.setDomainModelReference(domainModelReference);
		final VFeaturePathDomainModelReference tableDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		tableDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getTableWithoutMultiplicityConcrete_Content());
		domainModelReference.setDomainModelReference(tableDMR);
		final VFeaturePathDomainModelReference column = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();

		domainModelReference.getColumnDomainModelReferences().add(column);
		column.setDomainModelEFeature(TestPackage.eINSTANCE.getTableContentWithInnerChild_Stuff());

		final TableWithoutMultiplicityConcrete tableWithoutMultiplicity = TestFactory.eINSTANCE
			.createTableWithoutMultiplicityConcrete();
		final TableContentWithInnerChild child = TestFactory.eINSTANCE.createTableContentWithInnerChild();
		tableWithoutMultiplicity.getContent().add(child);
		final TableContentWithInnerChild2 innerChild = TestFactory.eINSTANCE.createTableContentWithInnerChild2();
		child.setInnerChild(innerChild);
		final TableContentWithValidation innerInnerChild = TestFactory.eINSTANCE.createTableContentWithValidation();
		innerChild.setInnerChild(innerInnerChild);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, tableWithoutMultiplicity);

		// TODO the table must be rendered in order to show diagnostic, bug?

		assertEquals(1, tableControl.getDiagnostic().getDiagnostics().size());
		assertEquals(Diagnostic.ERROR, tableControl.getDiagnostic().getHighestSeverity());

		// assertEquals(1, control.getDiagnostic().getDiagnostics().size());
		// assertEquals(Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());

		innerInnerChild.setName("a");
		assertEquals(0, tableControl.getDiagnostic().getDiagnostics().size());
		assertEquals(Diagnostic.OK, tableControl.getDiagnostic().getHighestSeverity());
		// assertEquals(0, control.getDiagnostic().getDiagnostics().size());
		// assertEquals(Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}
}
