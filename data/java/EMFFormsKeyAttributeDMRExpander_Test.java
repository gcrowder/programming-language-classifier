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
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyAttributeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emfforms.spi.common.report.AbstractReport;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDMRExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDomainExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsExpandingFailedException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFFormsKeyAttributeDMRExpander}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsKeyAttributeDMRExpander_Test {

	private EMFFormsKeyAttributeDMRExpander keyAttributeDMRExpander;
	private ReportService reportService;
	private EMFFormsDomainExpander domainExpander;
	private EMFFormsDatabinding databindingService;

	/**
	 * Creates a new {@link EMFFormsKeyAttributeDMRExpander} and mocks its required services for every test case.
	 */
	@Before
	public void setUp() {
		keyAttributeDMRExpander = new EMFFormsKeyAttributeDMRExpander();
		reportService = mock(ReportService.class);
		domainExpander = mock(EMFFormsDomainExpander.class);
		databindingService = mock(EMFFormsDatabinding.class);
		keyAttributeDMRExpander.setReportService(reportService);
		keyAttributeDMRExpander.setEMFFormsDomainExpander(domainExpander);
		keyAttributeDMRExpander.setEMFFormsDatabinding(databindingService);
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		// TODO remove?
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		assertEquals(5d, keyAttributeDMRExpander.isApplicable(mock(VKeyAttributeDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableNull() {
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE, keyAttributeDMRExpander.isApplicable(null), 0d);
		verify(reportService).report(any(AbstractReport.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test()
	public void testIsApplicableWrongReferenceType() {
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE,
			keyAttributeDMRExpander.isApplicable(mock(VFeaturePathDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectReferenceNull() throws EMFFormsExpandingFailedException {
		keyAttributeDMRExpander.prepareDomainObject(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectObjectNull() throws EMFFormsExpandingFailedException {
		keyAttributeDMRExpander.prepareDomainObject(mock(VKeyAttributeDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectBothNull() throws EMFFormsExpandingFailedException {
		keyAttributeDMRExpander.prepareDomainObject(null, null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.keyattribute.EMFFormsKeyAttributeDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectWrongReferenceType() throws EMFFormsExpandingFailedException {
		keyAttributeDMRExpander.prepareDomainObject(mock(VFeaturePathDomainModelReference.class), mock(EObject.class));
	}
}
