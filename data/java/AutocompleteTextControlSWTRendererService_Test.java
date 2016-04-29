/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.swt.control.text.autocomplete.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.databinding.EMFObservables;
import org.eclipse.emf.databinding.EMFProperties;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.swt.core.di.EMFFormsDIRendererService;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class AutocompleteTextControlSWTRendererService_Test {

	private static final String ANNOTATION_SOURCE = "org.eclipse.emfforms"; //$NON-NLS-1$
	private static final String ANNOTATION_KEY = "autocomplete"; //$NON-NLS-1$
	private static final String ANNOTATION_VALUE = "true"; //$NON-NLS-1$

	private DefaultRealm defaultRealm;

	private AutocompleteTextControlSWTRendererService rendererService;
	private ViewModelContext viewModelContext;
	private D domainModel;
	private VControl control;
	private EMFFormsDatabinding emfFormsDatabinding;

	@Before
	public void setUp() throws DatabindingFailedException {
		defaultRealm = new DefaultRealm();

		domainModel = TestFactory.eINSTANCE.createD();

		control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(TestPackage.eINSTANCE.getD_X());

		viewModelContext = mock(ViewModelContext.class);
		when(viewModelContext.getDomainModel()).thenReturn(domainModel);

		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getObservableValue(control.getDomainModelReference(), domainModel))
			.thenReturn(EMFObservables.observeValue(domainModel, TestPackage.eINSTANCE.getD_X()));

		rendererService = new AutocompleteTextControlSWTRendererService();
		rendererService.setDatabinding(emfFormsDatabinding);
	}

	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	@Test
	public void testGetRendererClass() {
		assertSame(AutocompleteTextControlSWTRenderer.class, rendererService.getRendererClass());
	}

	@Test
	public void testHasAutoCompleteAnnotation() {
		final EAttribute attribute = EcoreFactory.eINSTANCE.createEAttribute();
		final EAnnotation annotation = EcoreFactory.eINSTANCE.createEAnnotation();
		annotation.setSource(ANNOTATION_SOURCE);
		annotation.getDetails().put(ANNOTATION_KEY, ANNOTATION_VALUE);
		attribute.getEAnnotations().add(annotation);
		assertTrue(rendererService.hasAutoCompleteAnnotation(attribute));
	}

	@Test
	public void testHasAutoCompleteAnnotationInvalidValue() {
		final EAttribute attribute = EcoreFactory.eINSTANCE.createEAttribute();
		final EAnnotation annotation = EcoreFactory.eINSTANCE.createEAnnotation();
		annotation.setSource(ANNOTATION_SOURCE);
		annotation.getDetails().put(ANNOTATION_KEY, "false"); //$NON-NLS-1$
		attribute.getEAnnotations().add(annotation);
		assertFalse(rendererService.hasAutoCompleteAnnotation(attribute));
	}

	@Test
	public void testHasAutoCompleteAnnotationNullValue() {
		final EAttribute attribute = EcoreFactory.eINSTANCE.createEAttribute();
		final EAnnotation annotation = EcoreFactory.eINSTANCE.createEAnnotation();
		annotation.setSource(ANNOTATION_SOURCE);
		annotation.getDetails().put(ANNOTATION_KEY, null);
		attribute.getEAnnotations().add(annotation);
		assertFalse(rendererService.hasAutoCompleteAnnotation(attribute));
	}

	@Test
	public void testHasAutoCompleteAnnotationEmptyValue() {
		final EAttribute attribute = EcoreFactory.eINSTANCE.createEAttribute();
		final EAnnotation annotation = EcoreFactory.eINSTANCE.createEAnnotation();
		annotation.setSource(ANNOTATION_SOURCE);
		annotation.getDetails().put(ANNOTATION_KEY, ""); //$NON-NLS-1$
		attribute.getEAnnotations().add(annotation);
		assertFalse(rendererService.hasAutoCompleteAnnotation(attribute));
	}

	@Test
	public void testHasAutoCompleteAnnotationMissingKey() {
		final EAttribute attribute = EcoreFactory.eINSTANCE.createEAttribute();
		final EAnnotation annotation = EcoreFactory.eINSTANCE.createEAnnotation();
		annotation.setSource(ANNOTATION_SOURCE);
		attribute.getEAnnotations().add(annotation);
		assertFalse(rendererService.hasAutoCompleteAnnotation(attribute));
	}

	@Test
	public void testHasAutoCompleteAnnotationNoAnnotation() {
		final EAttribute attribute = EcoreFactory.eINSTANCE.createEAttribute();
		assertFalse(rendererService.hasAutoCompleteAnnotation(attribute));
	}

	@Test
	public void testIsApplicableNoControl() {
		final VElement element = mock(VElement.class);
		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(element, viewModelContext),
			0d);
	}

	@Test
	public void testIsApplicableNoDMR() {
		control = VViewFactory.eINSTANCE.createControl();
		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(control, viewModelContext),
			0d);
	}

	@Test
	public void testIsApplicableDataBindingFailed() throws DatabindingFailedException {
		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getValueProperty(control.getDomainModelReference(), domainModel))
			.thenThrow(new DatabindingFailedException("")); //$NON-NLS-1$
		rendererService.setDatabinding(emfFormsDatabinding);
		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(control, viewModelContext),
			0d);
	}

	@Test
	public void testIsApplicableNoAnnotation() throws DatabindingFailedException {
		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getValueProperty(control.getDomainModelReference(), domainModel))
			.thenReturn(EMFProperties.value(TestPackage.eINSTANCE.getD_X()));
		rendererService.setDatabinding(emfFormsDatabinding);

		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(control, viewModelContext),
			0d);
	}

	@Test
	public void testIsApplicableMulti() throws DatabindingFailedException {
		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getValueProperty(control.getDomainModelReference(), domainModel))
			.thenReturn(EMFProperties.value(TestPackage.eINSTANCE.getD_YList()));
		rendererService.setDatabinding(emfFormsDatabinding);

		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(control, viewModelContext),
			0d);
	}

	@Test
	public void testIsApplicableEReference() throws DatabindingFailedException {
		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getValueProperty(control.getDomainModelReference(), domainModel))
			.thenReturn(EMFProperties.value(TestPackage.eINSTANCE.getA_B()));
		rendererService.setDatabinding(emfFormsDatabinding);

		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(control, viewModelContext),
			0d);

	}

	@Test
	public void testIsApplicableNoString() throws DatabindingFailedException {

		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getValueProperty(control.getDomainModelReference(), domainModel))
			.thenReturn(
				EMFProperties.value(EcorePackage.eINSTANCE.getEAttribute_ID()));
		rendererService.setDatabinding(emfFormsDatabinding);

		assertEquals(EMFFormsDIRendererService.NOT_APPLICABLE, rendererService.isApplicable(control, viewModelContext),
			0d);
	}

	@Test
	public void testIsApplicable() throws DatabindingFailedException {
		final EAnnotation annotation = EcoreFactory.eINSTANCE.createEAnnotation();
		annotation.setSource(ANNOTATION_SOURCE);
		annotation.getDetails().put(ANNOTATION_KEY, ANNOTATION_VALUE);

		final EAttribute attributeSpy = EcoreUtil.copy(TestPackage.eINSTANCE.getD_X());
		attributeSpy.getEAnnotations().add(annotation);

		emfFormsDatabinding = mock(EMFFormsDatabinding.class);
		when(emfFormsDatabinding.getValueProperty(control.getDomainModelReference(), domainModel))
			.thenReturn(
				EMFProperties.value(attributeSpy));
		rendererService.setDatabinding(emfFormsDatabinding);

		assertEquals(3d, rendererService.isApplicable(control, viewModelContext), 0d);
	}

}
