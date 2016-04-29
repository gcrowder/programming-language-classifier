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
 * Johannes Faltermeier
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.validation.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.core.ECPProject;
import org.eclipse.emf.ecp.core.exceptions.ECPProjectWithNameExistsException;
import org.eclipse.emf.ecp.core.util.ECPUtil;
import org.eclipse.emf.ecp.emfstore.core.internal.EMFStoreProvider;
import org.eclipse.emf.ecp.validation.api.IValidationService;
import org.eclipse.emf.ecp.validation.api.IValidationServiceProvider;
import org.eclipse.emf.ecp.validation.test.test.Library;
import org.eclipse.emf.ecp.validation.test.test.TestFactory;
import org.eclipse.emf.ecp.validation.test.test.TestPackage;
import org.eclipse.emf.ecp.validation.test.test.Writer;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

@SuppressWarnings("restriction")
public class Validation_PTest {

	private static IValidationServiceProvider validationServiceProvider;

	@BeforeClass
	public static void setUpBeforeClass() {

		final BundleContext bundleContext = FrameworkUtil.getBundle(Validation_PTest.class).getBundleContext();
		final ServiceReference<?> eventServiceReference = bundleContext
			.getServiceReference(IValidationServiceProvider.class
				.getName());
		if (eventServiceReference != null) {
			validationServiceProvider = (IValidationServiceProvider) bundleContext.getService(eventServiceReference);
		}
	}

	@AfterClass
	public static void tearDownAfterClass() {
	}

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testSingleObject() {

		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final IValidationService validationService = validationServiceProvider.getValidationService(writer);

		validationService.validate(writer);
		final Diagnostic diagnostic = validationService.getDiagnostic(writer);
		assertEquals(Diagnostic.ERROR, diagnostic.getSeverity());
		assertEquals(1, diagnostic.getChildren().size());
		assertEquals(2, diagnostic.getChildren().get(0).getData().size());
		assertEquals(TestPackage.eINSTANCE.getWriter_FirstName(), diagnostic.getChildren().get(0).getData().get(1));
	}

	@Test
	public void testCorrectValidation() {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final IValidationService validationService = validationServiceProvider.getValidationService(writer);
		writer.setFirstName("Test");
		validationService.validate(writer);
		final Diagnostic diagnostic = validationService.getDiagnostic(writer);
		assertEquals(Diagnostic.OK, diagnostic.getSeverity());
	}

	@Test
	public void testPropagation() {
		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		library.setName("TesLib");
		library.getWriters().add(writer);

		final IValidationService validationService = validationServiceProvider.getValidationService(library);

		final Set<EObject> affectedElements = validationService.validate(writer);
		assertEquals(1, affectedElements.size());

		final Diagnostic diagnosticWriter = validationService.getDiagnostic(writer);
		assertEquals(Diagnostic.ERROR, diagnosticWriter.getSeverity());

		final Diagnostic diagnosticLib = validationService.getDiagnostic(library);
		assertEquals(Diagnostic.ERROR, diagnosticLib.getSeverity());

		assertEquals(1, diagnosticLib.getChildren().size());
		assertEquals(2, diagnosticLib.getChildren().get(0).getData().size());
		assertEquals(TestPackage.eINSTANCE.getWriter_FirstName(), diagnosticLib.getChildren().get(0).getData().get(1));
	}

	@Test
	public void testMultipleObjects() {
		final Writer writer1 = TestFactory.eINSTANCE.createWriter();
		writer1.setFirstName("Hans");
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		final Writer writer3 = TestFactory.eINSTANCE.createWriter();

		final Library lib = TestFactory.eINSTANCE.createLibrary();
		lib.setName("Bücherei");
		lib.getWriters().add(writer1);
		lib.getWriters().add(writer2);
		lib.getWriters().add(writer3);

		final Set<EObject> writers = new HashSet<EObject>();
		writers.add(writer1);
		writers.add(writer2);

		final IValidationService validationService = validationServiceProvider.getValidationService(lib);
		validationService.validate(writers);

		final Diagnostic diagnosticW1 = validationService.getDiagnostic(writer1);
		assertEquals(Diagnostic.OK, diagnosticW1.getSeverity());

		final Diagnostic diagnosticW2 = validationService.getDiagnostic(writer2);
		assertEquals(Diagnostic.ERROR, diagnosticW2.getSeverity());
		assertEquals(1, diagnosticW2.getChildren().size());
		assertEquals(2, diagnosticW2.getChildren().get(0).getData().size());
		assertEquals(TestPackage.eINSTANCE.getWriter_FirstName(), diagnosticW2.getChildren().get(0).getData().get(1));

		final Diagnostic diagnosticW3 = validationService.getDiagnostic(writer3);
		// TODO add correct assert
		// fail("When there is no (cached) value for a object you want to get a diagnostic for, "
		// + "returning OK may not be the best idea?! Return CANCEL maybe? discuss");
		assertEquals(Diagnostic.OK, diagnosticW3.getSeverity());
	}

	@Test
	public void testRootDiagnostic() {
		final Writer writer1 = TestFactory.eINSTANCE.createWriter();
		final Writer writer2 = TestFactory.eINSTANCE.createWriter();
		writer2.setFirstName("Hans");
		final Library library = TestFactory.eINSTANCE.createLibrary();
		library.setName("Bücherei");
		library.getWriters().add(writer1);
		library.getWriters().add(writer2);

		final Set<EObject> collection = new HashSet<EObject>();
		collection.add(library);
		collection.add(writer1);
		collection.add(writer2);

		final IValidationService validationService = validationServiceProvider.getValidationService(library);
		validationService.validate(collection);

		Diagnostic diagnostic = validationService.getRootDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getSeverity());

		writer1.setFirstName("Sepp");
		validationService.validate(collection);
		diagnostic = validationService.getRootDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getSeverity());

		// warning when firstname is same as lastname
		writer1.setLastName("Sepp");
		validationService.validate(collection);
		diagnostic = validationService.getRootDiagnostic();
		assertEquals(Diagnostic.WARNING, diagnostic.getSeverity());

		writer2.setFirstName("");
		validationService.validate(collection);
		diagnostic = validationService.getRootDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getSeverity());
	}

	@Test
	public void testECPProject() {
		try {
			final ECPProject project = ECPUtil.getECPProjectManager().createProject(
				ECPUtil.getECPProviderRegistry().getProvider(EMFStoreProvider.NAME), "Project");

			final Writer correctWriter = TestFactory.eINSTANCE.createWriter();
			correctWriter.setFirstName("Hans");
			final Writer errorWriter = TestFactory.eINSTANCE.createWriter();

			project.getContents().add(errorWriter);
			project.getContents().add(correctWriter);

			final IValidationService validationService = validationServiceProvider.getValidationService(project);

			validationService.validate(correctWriter);
			assertEquals(Diagnostic.OK, validationService.getDiagnostic(correctWriter).getSeverity());

			validationService.validate(errorWriter);
			final Diagnostic diagnostic = validationService.getDiagnostic(errorWriter);
			assertEquals(Diagnostic.ERROR, diagnostic.getSeverity());
			assertEquals(1, diagnostic.getChildren().size());
			assertEquals(2, diagnostic.getChildren().get(0).getData().size());
			assertEquals(TestPackage.eINSTANCE.getWriter_FirstName(), diagnostic.getChildren().get(0).getData().get(1));

		} catch (final ECPProjectWithNameExistsException ex) {
		}
	}

	@Test
	public void testAlreadyInMapping() {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		final IValidationService validationService1 = validationServiceProvider.getValidationService(writer);
		final IValidationService validationService2 = validationServiceProvider.getValidationService(writer);
		assertEquals(validationService1, validationService2);

		validationServiceProvider.deleteValidationService(writer);
		final IValidationService validationService3 = validationServiceProvider.getValidationService(writer);
		assertTrue(!validationService3.equals(validationService1));
	}
}
