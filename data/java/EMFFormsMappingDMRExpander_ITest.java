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
package org.eclipse.emfforms.internal.core.services.domainexpander.mapping;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collection;

import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingDomainModelReference;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
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
 * JUnit integration test cases for {@link EMFFormsMappingDMRExpander}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsMappingDMRExpander_ITest {

	private static BundleContext bundleContext;
	private ServiceReference<EMFFormsDMRExpander> serviceReference;
	private EMFFormsMappingDMRExpander mappingDMRExpander;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(EMFFormsMappingDMRExpander_ITest.class).getBundleContext();

	}

	@Before
	public void setUp() throws InvalidSyntaxException {
		final Collection<ServiceReference<EMFFormsDMRExpander>> serviceReferences = bundleContext.getServiceReferences(
			EMFFormsDMRExpander.class, null);
		mappingDMRExpander = null;
		serviceReference = null;
		for (final ServiceReference<EMFFormsDMRExpander> curRef : serviceReferences) {
			final EMFFormsDMRExpander curService = bundleContext.getService(curRef);
			if (EMFFormsMappingDMRExpander.class.isInstance(curService)) {
				mappingDMRExpander = (EMFFormsMappingDMRExpander) curService;
				serviceReference = curRef;
				break;
			}
			bundleContext.ungetService(curRef);
		}

		if (mappingDMRExpander == null) {
			fail("The EMFFormsMappingDMRExpander could not be found."); //$NON-NLS-1$
		}
	}

	@After
	public void tearDown() {
		bundleContext.ungetService(serviceReference);
	}

	@Test
	public void testServiceType() {
		assertTrue(EMFFormsMappingDMRExpander.class.isInstance(mappingDMRExpander));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test
	public void testPrepareDomainObject() throws EMFFormsExpandingFailedException {
		final VMappingDomainModelReference mappingDMR = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getB_C());
		mappingDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());

		mappingDMR.setDomainModelReference(targetDMR);
		mappingDMR.setMappedClass(TestPackage.eINSTANCE.getA());

		final B domainObject = TestFactory.eINSTANCE.createB();

		mappingDMRExpander.prepareDomainObject(mappingDMR, domainObject);
		assertNotNull(domainObject.getC());
		final A mapValue = domainObject.getC().getEClassToA().get(TestPackage.eINSTANCE.getA());
		assertNotNull(mapValue);
		assertNotNull(mapValue.getB());
	}
}
