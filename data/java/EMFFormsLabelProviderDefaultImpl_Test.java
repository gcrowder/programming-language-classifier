/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.core.services.label;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.databinding.EObjectObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.edit.provider.IItemPropertyDescriptor;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.emfspecificservice.EMFSpecificService;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit tests for {@link EMFFormsLabelProviderImpl}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsLabelProviderDefaultImpl_Test {

	private EMFFormsLabelProviderDefaultImpl labelProvider;
	private EMFSpecificService emfSpecificService;
	private IItemPropertyDescriptor itemPropertyDescriptor;
	private IValueProperty valueProperty;
	private EObjectObservableValue observableValue;
	private EMFFormsDatabinding databindingService;
	private DefaultRealm defaultRealm;

	/**
	 * Set up that is executed before every test case.
	 * Registers a databinding and an emf specific service.
	 * Mocks various objects for the tests.
	 */
	@Before
	public void setUp() {
		defaultRealm = new DefaultRealm();
		labelProvider = new EMFFormsLabelProviderDefaultImpl();

		databindingService = mock(EMFFormsDatabinding.class);
		labelProvider.setEMFFormsDatabinding(databindingService);

		valueProperty = mock(IValueProperty.class);
		observableValue = mock(EObjectObservableValue.class);
		// when(observableValue.getRealm()).thenReturn(Realm.getDefault());

		emfSpecificService = mock(EMFSpecificService.class);
		labelProvider.setEMFSpecificService(emfSpecificService);

		itemPropertyDescriptor = mock(IItemPropertyDescriptor.class);
		when(emfSpecificService.getIItemPropertyDescriptor(any(EObject.class), any(EStructuralFeature.class)))
			.thenReturn(itemPropertyDescriptor);
	}

	/**
	 * Clean up the realm.
	 */
	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(EStructuralFeature)}
	 * .
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 */
	@Test
	public void testGetDisplayNameOneParamFeature() throws DatabindingFailedException {
		final String expectedResult = "expected"; //$NON-NLS-1$
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);

		when(structuralFeature.getEContainingClass()).thenReturn(TestPackage.eINSTANCE.getD());
		when(itemPropertyDescriptor.getDisplayName(any(Object.class))).thenReturn(expectedResult);
		when(valueProperty.getValueType()).thenReturn(structuralFeature);

		final String result = labelProvider.getDisplayName(structuralFeature);

		verify(itemPropertyDescriptor).getDisplayName(any(D.class));
		assertEquals(expectedResult, result);
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 */
	@Test
	public void testGetDisplayNameOneParam() throws DatabindingFailedException {
		final String expectedResult = "expected"; //$NON-NLS-1$
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);

		when(structuralFeature.getEContainingClass()).thenReturn(TestPackage.eINSTANCE.getD());
		when(itemPropertyDescriptor.getDisplayName(any(Object.class))).thenReturn(expectedResult);
		when(valueProperty.getValueType()).thenReturn(structuralFeature);
		when(databindingService.getValueProperty(domainModelReference, null)).thenReturn(valueProperty);

		final IObservableValue result = labelProvider.getDisplayName(domainModelReference);

		verify(databindingService).getValueProperty(domainModelReference, null);
		verify(itemPropertyDescriptor).getDisplayName(any(D.class));
		assertEquals(expectedResult, result.getValue());
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDisplayNameOneParamNull() {
		labelProvider.getDisplayName((VDomainModelReference) null);
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 */
	@Test
	public void testGetDisplayNameTwoParams() throws DatabindingFailedException {
		final String expectedResult = "expected"; //$NON-NLS-1$
		final EObject eObject = mock(EObject.class);
		final EObject value = mock(EObject.class);
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);

		when(itemPropertyDescriptor.getDisplayName(value)).thenReturn(expectedResult);
		when(observableValue.getValueType()).thenReturn(structuralFeature);
		when(observableValue.getObserved()).thenReturn(value);
		when(databindingService.getObservableValue(domainModelReference, eObject)).thenReturn(observableValue);
		final IObservableValue result = labelProvider.getDisplayName(domainModelReference, eObject);

		verify(databindingService).getObservableValue(domainModelReference, eObject);
		verify(itemPropertyDescriptor).getDisplayName(value);
		assertEquals(expectedResult, result.getValue());
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDisplayNameTwoParamsReferenceNull() {
		labelProvider.getDisplayName(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDisplayNameTwoParamsObjectNull() {
		labelProvider.getDisplayName(mock(VDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDisplayNameTwoParamsBothNull() {
		labelProvider.getDisplayName(null, null);
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDescription(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 */
	@Test
	public void testGetDescriptionOneParam() throws DatabindingFailedException {
		final String expectedResult = "expected"; //$NON-NLS-1$
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);

		when(structuralFeature.getEContainingClass()).thenReturn(TestPackage.eINSTANCE.getD());
		when(itemPropertyDescriptor.getDescription(any(Object.class))).thenReturn(expectedResult);
		when(valueProperty.getValueType()).thenReturn(structuralFeature);
		when(databindingService.getValueProperty(domainModelReference, null)).thenReturn(valueProperty);

		final IObservableValue result = labelProvider.getDescription(domainModelReference);

		verify(databindingService).getValueProperty(domainModelReference, null);
		verify(itemPropertyDescriptor).getDescription(any(D.class));
		assertEquals(expectedResult, result.getValue());
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDescription(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDescriptionOneParamNull() {
		labelProvider.getDescription(null);
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDescription(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 */
	@Test
	public void testGetDescriptionTwoParams() throws DatabindingFailedException {
		final String expectedResult = "expected"; //$NON-NLS-1$
		final EObject eObject = mock(EObject.class);
		final EObject value = mock(EObject.class);
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);

		when(itemPropertyDescriptor.getDescription(value)).thenReturn(expectedResult);
		when(observableValue.getValueType()).thenReturn(structuralFeature);
		when(observableValue.getObserved()).thenReturn(value);
		when(databindingService.getObservableValue(domainModelReference, eObject)).thenReturn(observableValue);
		final IObservableValue result = labelProvider.getDescription(domainModelReference, eObject);

		verify(databindingService).getObservableValue(domainModelReference, eObject);
		verify(itemPropertyDescriptor).getDescription(value);
		assertEquals(expectedResult, result.getValue());
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDescription(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDescriptionTwoParamsReferenceNull() {
		labelProvider.getDescription(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDescription(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDescriptionTwoParamsObjectNull() {
		labelProvider.getDescription(mock(VDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link EMFFormsLabelProviderDefaultImpl#getDescription(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetDescriptionTwoParamsBothNull() {
		labelProvider.getDescription(null, null);
	}
}
