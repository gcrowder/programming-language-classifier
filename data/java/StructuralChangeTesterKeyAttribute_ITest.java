/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.core.services.structuralchange.keyattribute;

import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyAttributeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyattributedmrFactory;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeNotification;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.structuralchange.StructuralChangeTesterInternal;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;

/**
 * JUnit integration tests for {@link StructuralChangeTesterKeyAttribute}.
 *
 * @author Lucas Koehler
 *
 */
public class StructuralChangeTesterKeyAttribute_ITest {

	private static final String CORRECT_TEST_KEY = "CORRECT_TEST_KEY"; //$NON-NLS-1$
	private static BundleContext bundleContext;
	private StructuralChangeTesterInternal service;
	private ServiceReference<StructuralChangeTesterInternal> serviceReference;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(StructuralChangeTesterKeyAttribute_ITest.class)
			.getBundleContext();
	}

	@Before
	public void setUp() throws DatabindingFailedException, InvalidSyntaxException {
		final Collection<ServiceReference<StructuralChangeTesterInternal>> serviceReferences = bundleContext
			.getServiceReferences(StructuralChangeTesterInternal.class, null);
		for (final ServiceReference<StructuralChangeTesterInternal> testerRef : serviceReferences) {
			final StructuralChangeTesterInternal currentService = bundleContext.getService(testerRef);
			if (StructuralChangeTesterKeyAttribute.class.isInstance(currentService)) {
				service = currentService;
				serviceReference = testerRef;
			}
		}
		service = bundleContext.getService(serviceReference);

	}

	@After
	public void tearDown() {
		if (serviceReference != null) {
			bundleContext.ungetService(serviceReference);
		}
	}

	@Test
	public void testServicePresent() {
		assertTrue(StructuralChangeTesterKeyAttribute.class.isInstance(service));
	}

	/**
	 * This is the same test case as
	 * {@link StructuralChangeTesterKeyAttribute_Test#testIsStructureChangedKeyPresentAndValueDMRChanged}
	 * but with a real databinding and structural change tester.
	 */
	@Test
	public void testIntegration() {
		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D dWithKey = TestFactory.eINSTANCE.createD();
		dWithKey.setX(CORRECT_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(dWithKey);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(notification.getNotifier()).thenReturn(aOfC);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getA_B());

		final boolean result = service.isStructureChanged(keyAttributeDMR, a, notification);

		assertTrue(result);
	}

	/**
	 * @param keyDMR
	 * @param valueDMR
	 * @return
	 */
	private VKeyAttributeDomainModelReference createKeyAttributeDMR(final VFeaturePathDomainModelReference keyDMR,
		final VFeaturePathDomainModelReference valueDMR) {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);
		return keyAttributeDMR;
	}

	/**
	 * @return
	 */
	private VFeaturePathDomainModelReference createValueDMR() {
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());
		return valueDMR;
	}

	/**
	 * @return
	 */
	private VFeaturePathDomainModelReference createKeyDMR() {
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		return keyDMR;
	}
}
