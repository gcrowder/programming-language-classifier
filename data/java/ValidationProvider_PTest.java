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
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.internal.validation.ValidationNotification;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.validation.ValidationProvider;
import org.eclipse.emf.ecp.view.spi.validation.ValidationService;
import org.eclipse.emf.ecp.view.validation.test.model.Computer;
import org.eclipse.emf.ecp.view.validation.test.model.PowerBlock;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Eugen Neufeld
 *
 */
public class ValidationProvider_PTest {

	private DefaultRealm defaultRealm;

	private VControl control;
	private Computer computer;
	private ValidationService validationService;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		defaultRealm = new DefaultRealm();
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TestPackage.eINSTANCE.getComputer());

		control = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(TestPackage.eINSTANCE.getComputer_Name());
		control.setDomainModelReference(domainModelReference);

		view.getChildren().add(control);

		computer = TestFactory.eINSTANCE.createComputer();
		computer.setName("initial");
		final PowerBlock powerBlock = TestFactory.eINSTANCE.createPowerBlock();
		// powerBlock.setName("powerblock");
		computer.setPowerBlock(powerBlock);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, computer);
		validationService = vmc.getService(ValidationService.class);

	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		defaultRealm.dispose();
	}

	@Test
	public void testValidationProviderIsCalled() {
		final List<Boolean> called = new ArrayList<Boolean>(1);
		called.add(false);
		validationService.addValidationProvider(new ValidationProvider() {

			@Override
			public List<Diagnostic> validate(EObject eObject) {
				called.set(0, true);
				return Collections.emptyList();
			}
		});

		assertTrue(called.get(0));
	}

	@Test
	public void testValidationProviderSeverityHigher() {
		validationService.addValidationProvider(new ValidationProvider() {

			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (!Computer.class.isInstance(eObject)) {
					return Collections.emptyList();
				}
				final Diagnostic diagnostic = new BasicDiagnostic(Diagnostic.WARNING, "bla", 0, "bl", new Object[] {
					eObject, TestPackage.eINSTANCE.getComputer_Name() });
				return Collections.singletonList(diagnostic);
			}
		});

		assertEquals(Diagnostic.WARNING, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationProviderSeverityLower() {
		validationService.addValidationProvider(new ValidationProvider() {

			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (!Computer.class.isInstance(eObject)) {
					return Collections.emptyList();
				}
				final Diagnostic diagnostic = new BasicDiagnostic(Diagnostic.WARNING, "bla", 0, "bl", new Object[] {
					eObject, TestPackage.eINSTANCE.getComputer_Name() });
				return Collections.singletonList(diagnostic);
			}
		});
		computer.setName(null);

		assertEquals(Diagnostic.ERROR, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationProviderTriggerAdditionalValidation() {
		final List<Integer> called = new ArrayList<Integer>(1);
		called.add(0);
		validationService.addValidationProvider(new ValidationProvider() {

			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (Computer.class.isInstance(eObject)) {
					((Computer) eObject).eNotify(new ValidationNotification(((Computer) eObject).getPowerBlock()));
					final Diagnostic diagnostic = new BasicDiagnostic(Diagnostic.WARNING, "bla", 0, "bl", new Object[] {
						eObject, TestPackage.eINSTANCE.getComputer_Name() });
					return Collections.singletonList(diagnostic);
				}
				if (PowerBlock.class.isInstance(eObject)) {
					called.set(0, called.get(0) + 1);
				}
				return Collections.emptyList();
			}
		});
		computer.setName("test");
		assertEquals(Diagnostic.WARNING, control.getDiagnostic().getHighestSeverity());
		assertEquals((Integer) 2, called.get(0));
	}

	@Test
	public void testValidationProviderTriggerSelfValidation() {
		final List<Integer> called = new ArrayList<Integer>(1);
		called.add(0);
		validationService.addValidationProvider(new ValidationProvider() {

			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (Computer.class.isInstance(eObject)) {

					eObject.eNotify(new ValidationNotification(eObject));
					called.set(0, called.get(0) + 1);
				}
				return Collections.emptyList();
			}
		});
		assertEquals((Integer) 1, called.get(0));
	}

	@Test
	public void testRemoveValidationProvider() {
		// setup
		final ValidationProvider validationProvider = new ValidationProvider() {
			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (!Computer.class.isInstance(eObject)) {
					return Collections.emptyList();
				}
				final Diagnostic diagnostic = new BasicDiagnostic(Diagnostic.WARNING, "bla", 0, "bl", new Object[] {
					eObject, TestPackage.eINSTANCE.getComputer_Name() });
				return Collections.singletonList(diagnostic);
			}
		};
		validationService.addValidationProvider(validationProvider);
		assertEquals(Diagnostic.WARNING, control.getDiagnostic().getHighestSeverity());

		// act
		validationService.removeValidationProvider(validationProvider);
		assertEquals(Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationProviderTriggerNoSelfValidation() {
		final List<Integer> called = new ArrayList<Integer>(1);
		called.add(0);
		validationService.addValidationProvider(new ValidationProvider() {

			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (Computer.class.isInstance(eObject)) {

					eObject.eNotify(new ValidationNotification(eObject));
					called.set(0, called.get(0) + 1);
				}
				return Collections.emptyList();
			}
		}, false);
		assertEquals((Integer) 0, called.get(0));
	}

	@Test
	public void testRemoveValidationProviderNoRevalidation() {
		// setup
		final ValidationProvider validationProvider = new ValidationProvider() {
			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (!Computer.class.isInstance(eObject)) {
					return Collections.emptyList();
				}
				final Diagnostic diagnostic = new BasicDiagnostic(Diagnostic.WARNING, "bla", 0, "bl", new Object[] {
					eObject, TestPackage.eINSTANCE.getComputer_Name() });
				return Collections.singletonList(diagnostic);
			}
		};
		validationService.addValidationProvider(validationProvider);
		assertEquals(Diagnostic.WARNING, control.getDiagnostic().getHighestSeverity());

		// act
		validationService.removeValidationProvider(validationProvider, false);
		/* no revalidation yet */
		assertEquals(Diagnostic.WARNING, control.getDiagnostic().getHighestSeverity());
		/* revalidate to see if provider was removed */
		validationService.validate(Collections.singleton(EObject.class.cast(computer)));
		assertEquals(Diagnostic.OK, control.getDiagnostic().getHighestSeverity());
	}
}
