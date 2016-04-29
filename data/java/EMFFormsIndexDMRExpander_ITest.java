/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler- initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.core.services.domainexpander.index;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collection;

import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
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
 * JUnit integration tests for {@link EMFFormsIndexDMRExpander}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsIndexDMRExpander_ITest {

	private static BundleContext bundleContext;
	private ServiceReference<EMFFormsDMRExpander> serviceReference;
	private EMFFormsIndexDMRExpander indexDMRExpander;
	private DefaultRealm defaultRealm;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(EMFFormsIndexDMRExpander_ITest.class).getBundleContext();
	}

	@Before
	public void setUp() throws InvalidSyntaxException {
		defaultRealm = new DefaultRealm();
		final Collection<ServiceReference<EMFFormsDMRExpander>> serviceReferences = bundleContext.getServiceReferences(
			EMFFormsDMRExpander.class, null);
		indexDMRExpander = null;
		serviceReference = null;
		for (final ServiceReference<EMFFormsDMRExpander> curRef : serviceReferences) {
			final EMFFormsDMRExpander curService = bundleContext.getService(curRef);
			if (EMFFormsIndexDMRExpander.class.isInstance(curService)) {
				indexDMRExpander = (EMFFormsIndexDMRExpander) curService;
				serviceReference = curRef;
				break;
			}
			bundleContext.ungetService(curRef);
		}

		if (indexDMRExpander == null) {
			fail("The EMFFormsIndexDMRExpander could not be found."); //$NON-NLS-1$
		}
	}

	@After
	public void tearDown() {
		bundleContext.ungetService(serviceReference);
		defaultRealm.dispose();
	}

	@Test
	public void testServiceType() {
		assertTrue(EMFFormsIndexDMRExpander.class.isInstance(indexDMRExpander));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test
	public void testPrepareDomainObjectWithPrefixDMR() throws EMFFormsExpandingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		final VFeaturePathDomainModelReference prefixDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		prefixDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		prefixDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		indexDMR.setPrefixDMR(prefixDMR);
		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(1);

		final A domainObject = TestFactory.eINSTANCE.createA();

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
		assertNotNull(domainObject.getB());
		assertTrue(domainObject.getB().getCList().size() == 2);
		assertNotNull(domainObject.getB().getCList().get(0));
		assertNotNull(domainObject.getB().getCList().get(1));
		assertNotNull(domainObject.getB().getCList().get(1).getD());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test
	public void testPrepareDomainObjectWithoutPrefixDMR() throws EMFFormsExpandingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(1);

		final A domainObject = TestFactory.eINSTANCE.createA();

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
		assertNotNull(domainObject.getB());
		assertTrue(domainObject.getB().getCList().size() == 2);
		assertNotNull(domainObject.getB().getCList().get(0));
		assertNotNull(domainObject.getB().getCList().get(1));
		assertNotNull(domainObject.getB().getCList().get(1).getD());
	}
}
