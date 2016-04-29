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
package org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collection;

import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyAttributeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyattributedmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDMRExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsExpandingFailedException;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;

/**
 * Integration tests for {@link EMFFormsKeyAttributeDMRExpander}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsKeyAttributeDMRExpander_ITest {

	private static final String TEST_KEY = "TestKey"; //$NON-NLS-1$
	private static BundleContext bundleContext;
	private ServiceReference<EMFFormsDMRExpander> serviceReference;
	private EMFFormsKeyAttributeDMRExpander keyAttributeDMRExpander;
	private DefaultRealm defaultRealm;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(EMFFormsKeyAttributeDMRExpander_ITest.class).getBundleContext();
	}

	@Before
	public void setUp() throws InvalidSyntaxException {
		defaultRealm = new DefaultRealm();
		final Collection<ServiceReference<EMFFormsDMRExpander>> serviceReferences = bundleContext.getServiceReferences(
			EMFFormsDMRExpander.class, null);
		keyAttributeDMRExpander = null;
		serviceReference = null;

		for (final ServiceReference<EMFFormsDMRExpander> curRef : serviceReferences) {
			final EMFFormsDMRExpander curService = bundleContext.getService(curRef);
			if (EMFFormsKeyAttributeDMRExpander.class.isInstance(curService)) {
				keyAttributeDMRExpander = (EMFFormsKeyAttributeDMRExpander) curService;
				serviceReference = curRef;
				break;
			}
			bundleContext.ungetService(curRef);
		}

		if (keyAttributeDMRExpander == null) {
			fail("The EMFFormsKeyAttributeDMRExpander could not be found."); //$NON-NLS-1$
		}
	}

	@After
	public void tearDown() {
		if (serviceReference != null) {
			bundleContext.ungetService(serviceReference);
		}
		defaultRealm.dispose();
	}

	@Test
	public void testServiceType() {
		assertTrue(EMFFormsKeyAttributeDMRExpander.class.isInstance(keyAttributeDMRExpander));
	}

	@Test
	public void testPrepareDomainObjectFeaturePathCompleteAndKeyNotPresent() throws EMFFormsExpandingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();

		a.setB(b);
		b.getCList().add(c);

		assertNull(c.getA());
		assertNull(c.getD());

		keyAttributeDMRExpander.prepareDomainObject(keyAttributeDMR, a);

		// Neither the "key path" nor the "value path" for c must be expanded because the key path must never be
		// expanded and c does not have the key value at the end of the key dmr.
		assertNull(c.getA());
		assertNull(c.getD());
		assertTrue(b.getCList().contains(c));
		assertEquals(1, b.getCList().size());
	}

	@Test
	public void testPrepareDomainObjectFeaturePathCompleteAndKeyPresent() throws EMFFormsExpandingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithKey = TestFactory.eINSTANCE.createC();
		final C cWithoutKey = TestFactory.eINSTANCE.createC();
		final D d = TestFactory.eINSTANCE.createD();
		d.setX(TEST_KEY);

		a.setB(b);
		b.getCList().add(cWithKey);
		b.getCList().add(cWithoutKey);
		cWithKey.setD(d);

		assertNull(cWithKey.getA());
		assertNull(cWithoutKey.getA());

		keyAttributeDMRExpander.prepareDomainObject(keyAttributeDMR, a);

		// The value path must be expanded for cWithKey and must not be expanded for cWithoutKey.
		assertNotNull(cWithKey.getA());
		assertNull(cWithKey.getA().getB());
		assertNotNull(cWithKey.getD());
		assertNull(cWithoutKey.getA());

		assertEquals(2, b.getCList().size());
	}

	@Test
	public void testPrepareDomainObjectFeaturePathIncomplete() throws EMFFormsExpandingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final A a = TestFactory.eINSTANCE.createA();

		assertNull(a.getB());

		keyAttributeDMRExpander.prepareDomainObject(keyAttributeDMR, a);

		// No element must be added to the multi-reference of the feature path part.
		assertNotNull(a.getB());
		assertTrue(a.getB().getCList().isEmpty());
	}

	@Test
	public void testPrepareDomainObjectFeaturePathMultireferenceEmpty() throws EMFFormsExpandingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();

		a.setB(b);

		assertTrue(b.getCList().isEmpty());

		keyAttributeDMRExpander.prepareDomainObject(keyAttributeDMR, a);

		// No element must be added to the multi-reference of the feature path part.
		assertTrue(a.getB().getCList().isEmpty());
	}
}
