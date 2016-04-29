/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Stefan Dirix - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.validation.ValidationProvider;
import org.eclipse.emf.ecp.view.spi.validation.ValidationService;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContainer;
import org.eclipse.emf.ecp.view.validation.test.model.CrossReferenceContent;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Stefan Dirix
 *
 */
public class ValidationService_PTest {

	private DefaultRealm defaultRealm;

	private VControl control;
	private ValidationService validationService;

	private CrossReferenceContainer container;
	private CrossReferenceContent content;

	private CrossReferenceContainer otherContainer;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		defaultRealm = new DefaultRealm();
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		defaultRealm.dispose();
	}

	private void setupContent() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TestPackage.eINSTANCE.getContent());

		control = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(TestPackage.eINSTANCE.getCrossReferenceContent_Parent());
		control.setDomainModelReference(domainModelReference);

		view.getChildren().add(control);

		container = TestFactory.eINSTANCE.createCrossReferenceContainer();
		otherContainer = TestFactory.eINSTANCE.createCrossReferenceContainer();
		content = TestFactory.eINSTANCE.createCrossReferenceContent();
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, content);
		validationService = vmc.getService(ValidationService.class);
	}

	/**
	 * There should be no validation on an unset crossreferenced element.
	 */
	@Test
	public void testNoValidationOnUnsetElements() {
		setupContent();
		container.setSingleContent(content);
		final List<Boolean> called = new ArrayList<Boolean>(1);
		called.add(false);
		validationService.addValidationProvider(new ValidationProvider() {
			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (content == eObject) {
					called.set(0, true);
				}
				return Collections.emptyList();
			}
		});
		called.set(0, false);
		container.setSingleContent(null);

		assertFalse(called.get(0));
	}

	/**
	 * There should be a validation on a set crossreferenced element.
	 */
	@Test
	public void testValidationOnSetElements() {
		setupContent();
		container.setSingleContent(content);
		final List<Boolean> called = new ArrayList<Boolean>(1);
		called.add(false);
		validationService.addValidationProvider(new ValidationProvider() {
			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (content == eObject) {
					called.set(0, true);
				}
				return Collections.emptyList();
			}
		});
		called.set(0, false);
		otherContainer.setSingleContent(content);

		assertTrue(called.get(0));
	}

	/**
	 * There should be no validation on a removed crossreferenced element.
	 */
	@Test
	public void testNoValidationOnRemovedElements() {
		setupContent();
		container.getContents().add(content);

		final List<Boolean> called = new ArrayList<Boolean>(1);
		called.add(false);
		validationService.addValidationProvider(new ValidationProvider() {
			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (content == eObject) {
					called.set(0, true);
				}
				return Collections.emptyList();
			}
		});
		called.set(0, false);
		container.getContents().remove(content);

		assertFalse(called.get(0));
	}

	/**
	 * There should be a validation on a moved crossreferenced element.
	 */
	@Test
	public void testValidationOnMovedElements() {
		setupContent();
		container.getContents().add(content);

		final List<Boolean> called = new ArrayList<Boolean>(1);
		called.add(false);
		validationService.addValidationProvider(new ValidationProvider() {
			@Override
			public List<Diagnostic> validate(EObject eObject) {
				if (content == eObject) {
					called.set(0, true);
				}
				return Collections.emptyList();
			}
		});
		called.set(0, false);
		otherContainer.getContents().add(content);

		assertTrue(called.get(0));
	}

}
