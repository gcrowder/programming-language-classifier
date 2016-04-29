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
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.internal.core.services.label.BundleResolver.NoBundleFoundException;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.eclipse.emfforms.spi.localization.EMFFormsLocalizationService;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

/**
 * JUnit integration test that tests that {@link EMFFormsLabelProviderImpl} uses the correct services.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsLabelProviderImpl_ITest {

	private static BundleContext bundleContext;
	private static EMFFormsDatabinding databindingService;
	private static ServiceRegistration<EMFFormsDatabinding> databindingRegisterService;
	private static EMFFormsLabelProviderImpl labelProvider;
	private static ServiceReference<EMFFormsLabelProvider> serviceReference;
	private static EMFFormsLocalizationService localizationService;
	private static ServiceRegistration<EMFFormsLocalizationService> localizationServiceReference;
	private static IValueProperty valueProperty;
	private DefaultRealm realm;

	/**
	 * Set up that is executed before every test case.
	 * Registers a databinding and an emf specific service.
	 * Mocks various objects for the tests.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoBundleFoundException
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws DatabindingFailedException, NoBundleFoundException {
		bundleContext = FrameworkUtil.getBundle(EMFFormsLabelProviderImpl_Test.class).getBundleContext();

		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 5); //$NON-NLS-1$

		databindingService = mock(EMFFormsDatabinding.class);
		databindingRegisterService = bundleContext.registerService(EMFFormsDatabinding.class, databindingService,
			dictionary);

		localizationService = mock(EMFFormsLocalizationService.class);
		localizationServiceReference = bundleContext.registerService(EMFFormsLocalizationService.class,
			localizationService,
			dictionary);

		final EClass eContainingClass = TestPackage.eINSTANCE.getD();
		final EStructuralFeature structuralFeature = mock(EStructuralFeature.class);
		when(structuralFeature.getEContainingClass()).thenReturn(eContainingClass);
		when(structuralFeature.getName()).thenReturn("My Feature"); //$NON-NLS-1$
		valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(structuralFeature);
		final EObject eObject = mock(EObject.class);
		final EClass eClass = mock(EClass.class);
		when(eClass.getName()).thenReturn("My EClass"); //$NON-NLS-1$
		when(eObject.eClass()).thenReturn(eClass);
		when(databindingService.getValueProperty(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			valueProperty);

		final Bundle bundle = mock(Bundle.class);
		final BundleResolver bundleResolver = mock(BundleResolver.class);
		when(bundleResolver.getEditBundle(eContainingClass)).thenReturn(bundle);

		when(localizationService.getString(same(bundle), any(String.class))).thenReturn("Test"); //$NON-NLS-1$

		serviceReference = bundleContext.getServiceReference(EMFFormsLabelProvider.class);
		labelProvider = (EMFFormsLabelProviderImpl) bundleContext.getService(serviceReference);
		labelProvider.setBundleResolver(bundleResolver);
	}

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
	public static void tearDownAfterClass() {
		databindingRegisterService.unregister();
		localizationServiceReference.unregister();
		bundleContext.ungetService(serviceReference);
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderImpl#getDisplayName(VDomainModelReference)} uses the databinding and emf
	 * specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testServiceUsageDisplayNameOneParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		labelProvider.getDisplayName(domainModelReference);

		verify(databindingService).getValueProperty(same(domainModelReference), any(EObject.class));
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderImpl#getDescription(VDomainModelReference)} uses the databinding and emf
	 * specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testServiceUsageDescriptionOneParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		labelProvider.getDescription(domainModelReference);

		verify(databindingService).getValueProperty(same(domainModelReference), any(EObject.class));
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderImpl#getDisplayName(VDomainModelReference, EObject)} uses the databinding
	 * and emf specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testServiceUsageDisplayNameTwoParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		final EObject eObject = mock(EObject.class);
		labelProvider.getDisplayName(domainModelReference, eObject);

		verify(databindingService).getValueProperty(domainModelReference, eObject);
	}

	/**
	 * Tests that {@link EMFFormsLabelProviderImpl#getDescription(VDomainModelReference, EObject)} uses the databinding
	 * and emf specific services.
	 *
	 * @throws DatabindingFailedException should not happen, just needs to be thrown because the databinding service
	 *             defines the throw in its interface.
	 * @throws NoLabelFoundException
	 */
	@Test
	public void testServiceUsageDescriptionTwoParam() throws DatabindingFailedException, NoLabelFoundException {
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);
		final EObject eObject = mock(EObject.class);
		labelProvider.getDescription(domainModelReference, eObject);

		verify(databindingService).getValueProperty(domainModelReference, eObject);
	}
}
