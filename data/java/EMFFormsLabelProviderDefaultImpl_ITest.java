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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Dictionary;
import java.util.Hashtable;

import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.databinding.EObjectObservableValue;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.edit.provider.IItemPropertyDescriptor;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.emfspecificservice.EMFSpecificService;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

/**
 * JUnit integration test that tests that {@link EMFFormsLabelProviderDefaultImpl} uses the correct services.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsLabelProviderDefaultImpl_ITest {

	private static BundleContext bundleContext;
	private static EMFFormsDatabinding databindingService;
	private static ServiceRegistration<EMFFormsDatabinding> databindingRegisterService;
	private static EMFSpecificService emfSpecificService;
	private static ServiceRegistration<EMFSpecificService> emfSpecificRegisterService;
	private static EMFFormsLabelProviderDefaultImpl labelProvider;
	private static ServiceReference<EMFFormsLabelProvider> serviceReference;
	private static IValueProperty valueProperty;
	private static EObjectObservableValue observableValue;
	private static IItemPropertyDescriptor itemPropertyDescriptor;

	/**
	 * Set up that is executed before every test case.
	 * Registers a databinding and an emf specific service.
	 * Mocks various objects for the tests.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws InvalidSyntaxException thrown by OSGI
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws DatabindingFailedException, InvalidSyntaxException {
		bundleContext = FrameworkUtil.getBundle(EMFFormsLabelProviderDefaultImpl_ITest.class).getBundleContext();

		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 5); //$NON-NLS-1$

		databindingService = mock(EMFFormsDatabinding.class);
		databindingRegisterService = bundleContext.registerService(EMFFormsDatabinding.class, databindingService,
			dictionary);

		emfSpecificService = mock(EMFSpecificService.class);
		emfSpecificRegisterService = bundleContext.registerService(
			EMFSpecificService.class, emfSpecificService, dictionary);

		final EClass eContainingClass = TestPackage.eINSTANCE.getD();
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		when(structuralFeature.getEContainingClass()).thenReturn(eContainingClass);
		valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(structuralFeature);
		observableValue = mock(EObjectObservableValue.class);
		when(observableValue.getValueType()).thenReturn(structuralFeature);
		when(observableValue.getObserved()).thenReturn(mock(EObject.class));
		itemPropertyDescriptor = mock(IItemPropertyDescriptor.class);
		when(itemPropertyDescriptor.getDescription(any(EObject.class))).thenReturn("description"); //$NON-NLS-1$
		when(itemPropertyDescriptor.getDisplayName(any(EObject.class))).thenReturn("displayName"); //$NON-NLS-1$

		when(emfSpecificService.getIItemPropertyDescriptor(any(EObject.class), same(structuralFeature))).thenReturn(
			itemPropertyDescriptor);
		when(databindingService.getValueProperty(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			valueProperty);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			observableValue);

		serviceReference = bundleContext.getServiceReferences(EMFFormsLabelProvider.class, "(service.ranking=1)") //$NON-NLS-1$
			.iterator().next();
		labelProvider = (EMFFormsLabelProviderDefaultImpl) bundleContext.getService(serviceReference);
	}

	private DefaultRealm realm;

	/**
	 * Resets and newly configures the services for every test.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 */
	@Before
	public void setUp() throws DatabindingFailedException {
		reset(databindingService);
		when(databindingService.getValueProperty(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			valueProperty);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			observableValue);
		realm = new DefaultRealm();
	}

	/**
	 * Dispose the realm.
	 */
	@After
	public void tearDown() {
		realm.dispose();
	}

	/**
	 * Unregisters the services after every test.
	 */
	@AfterClass
	public static void tearDownClass() {
		databindingRegisterService.unregister();
		emfSpecificRegisterService.unregister();
		bundleContext.ungetService(serviceReference);
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(VDomainModelReference)} uses the databinding
	 * and emf
	 * specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException should not be thrown
	 */
	@Test
	public void testServiceUsageDisplayNameOneParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		labelProvider.getDisplayName(domainModelReference);

		verify(databindingService).getValueProperty(same(domainModelReference), any(EObject.class));
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderDefaultImpl#getDescription(VDomainModelReference)} uses the databinding
	 * and emf
	 * specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException should not be thrown
	 */
	@Test
	public void testServiceUsageDescriptionOneParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		labelProvider.getDescription(domainModelReference);

		verify(databindingService).getValueProperty(same(domainModelReference), any(EObject.class));
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderDefaultImpl#getDisplayName(VDomainModelReference, EObject)} uses the
	 * databinding
	 * and emf specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException Should not be thrown
	 */
	@Test
	public void testServiceUsageDisplayNameTwoParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		final EObject eObject = mock(EObject.class);
		labelProvider.getDisplayName(domainModelReference, eObject);

		verify(databindingService).getObservableValue(domainModelReference, eObject);
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderDefaultImpl#getDescription(VDomainModelReference, EObject)} uses the
	 * databinding
	 * and emf specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException Should not be thrown
	 */
	@Test
	public void testServiceUsageDescriptionTwoParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		final EObject eObject = mock(EObject.class);
		labelProvider.getDescription(domainModelReference, eObject);

		verify(databindingService).getObservableValue(domainModelReference, eObject);
	}
}
