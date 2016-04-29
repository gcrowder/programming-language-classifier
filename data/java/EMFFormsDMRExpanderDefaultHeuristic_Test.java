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
package org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDMRExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsExpandingFailedException;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFFormsDMRExpanderDefaultHeuristic}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsDMRExpanderDefaultHeuristic_Test {
	private EMFFormsDMRExpanderDefaultHeuristic domainExpander;

	/**
	 * Creates a new {@link EMFFormsDMRExpanderDefaultHeuristic} and sets its {@link ReportService} for every test case.
	 */
	@Before
	public void setUp() {
		domainExpander = new EMFFormsDMRExpanderDefaultHeuristic();
		domainExpander.setReportService(mock(ReportService.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		final VFeaturePathDomainModelReference reference = mock(VFeaturePathDomainModelReference.class);
		assertEquals(1d, domainExpander.isApplicable(reference), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableNotApplicable() {
		final VDomainModelReference reference = mock(VDomainModelReference.class);
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE, domainExpander.isApplicable(reference), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableNull() {
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE, domainExpander.isApplicable(null), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test
	public void testPrepareDomainObject() throws EMFFormsExpandingFailedException {
		final VFeaturePathDomainModelReference reference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		reference.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		reference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		final A a = TestFactory.eINSTANCE.createA();
		assertNull(a.getB());

		domainExpander.prepareDomainObject(reference, a);

		assertNotNull(a.getB());
		assertNull(a.getB().getC());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test
	public void testPrepareDomainObjectList() throws EMFFormsExpandingFailedException {
		final VFeaturePathDomainModelReference reference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		reference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_D());
		reference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		reference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getB_CList());
		final A a = TestFactory.eINSTANCE.createA();
		assertNull(a.getB());

		domainExpander.prepareDomainObject(reference, a);

		assertNotNull(a.getB());
		assertTrue(a.getB().getCList().isEmpty());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test(expected = EMFFormsExpandingFailedException.class)
	public void testPrepareDomainObjectEmptyReference() throws EMFFormsExpandingFailedException {
		final VFeaturePathDomainModelReference reference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		final A a = TestFactory.eINSTANCE.createA();

		domainExpander.prepareDomainObject(reference, a);

	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectReferenceNull() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectObjectNull() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(mock(VDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectBothNull() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(null, null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDMRExpanderDefaultHeuristic#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException if the expansion fails.
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectWrongReference() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(mock(VDomainModelReference.class), mock(EObject.class));
	}
}
