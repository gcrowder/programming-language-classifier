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
package org.eclipse.emfforms.internal.core.services.domainexpander.mapping;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingDomainModelReference;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.common.report.AbstractReport;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDMRExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDomainExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsExpandingFailedException;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFFormsMappingDMRExpander}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsMappingDMRExpander_Test {

	private EMFFormsMappingDMRExpander mappingExpander;
	private ReportService reportService;
	private EMFFormsDomainExpander domainExpander;

	@Before
	public void setUp() {
		mappingExpander = new EMFFormsMappingDMRExpander();
		reportService = mock(ReportService.class);
		domainExpander = mock(EMFFormsDomainExpander.class);
		mappingExpander.setReportService(reportService);
		mappingExpander.setEMFFormsDomainExpander(domainExpander);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectReferenceNull() throws EMFFormsExpandingFailedException {
		mappingExpander.prepareDomainObject(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectObjectNull() throws EMFFormsExpandingFailedException {
		mappingExpander.prepareDomainObject(mock(VMappingDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectBothNull() throws EMFFormsExpandingFailedException {
		mappingExpander.prepareDomainObject(null, null);
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
		/*
		 * Note: Testing in JUnit tests is afaik only possible without using domain model e reference paths because
		 * they get expanded by the EMFFormsDomainExpander that has to be mocked in a JUnit test.
		 */

		final VMappingDomainModelReference mappingDMR = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		mappingDMR.setDomainModelReference(targetDMR);
		mappingDMR.setMappedClass(TestPackage.eINSTANCE.getA());

		final C domainObject = TestFactory.eINSTANCE.createC();

		mappingExpander.prepareDomainObject(mappingDMR, domainObject);
		assertNotNull(domainObject.getEClassToA().get(TestPackage.eINSTANCE.getA()));
		verify(domainExpander, times(2)).prepareDomainObject(any(VDomainModelReference.class), any(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = EMFFormsExpandingFailedException.class)
	public void testPrepareDomainObjectNoMap() throws EMFFormsExpandingFailedException {
		/*
		 * Note: Testing in JUnit tests is afaik only possible without using domain model e reference paths because
		 * they get expanded by the EMFFormsDomainExpander that has to be mocked in a JUnit test.
		 */

		final VMappingDomainModelReference mappingDMR = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getC_D());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		mappingDMR.setDomainModelReference(targetDMR);
		mappingDMR.setMappedClass(TestPackage.eINSTANCE.getD());

		final C domainObject = TestFactory.eINSTANCE.createC();

		mappingExpander.prepareDomainObject(mappingDMR, domainObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		final VMappingDomainModelReference reference = mock(VMappingDomainModelReference.class);
		assertEquals(5d, mappingExpander.isApplicable(reference), 0d);

	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableNull() {
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE, mappingExpander.isApplicable(null), 0d);
		verify(reportService).report(any(AbstractReport.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.mapping.EMFFormsMappingDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableWrongReferenceType() {
		final VDomainModelReference reference = mock(VDomainModelReference.class);
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE, mappingExpander.isApplicable(reference), 0d);
	}
}
