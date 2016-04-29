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
package org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDMRExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsExpandingFailedException;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFFormsDomainExpanderImpl}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsDomainExpanderImpl_Test {

	private EMFFormsDomainExpanderImpl domainExpander;

	/**
	 * Create a new {@link EMFFormsDomainExpanderImpl} for every test case.
	 */
	@Before
	public void setUp() {
		domainExpander = new EMFFormsDomainExpanderImpl();
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = EMFFormsExpandingFailedException.class)
	public void testPrepareDomainObjectNoDMRExpander() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(mock(VDomainModelReference.class), mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = EMFFormsExpandingFailedException.class)
	public void testPrepareDomainObjectNoSuitableDMRExpander() throws EMFFormsExpandingFailedException {
		final EMFFormsDMRExpander dmrExpander = mock(EMFFormsDMRExpander.class);
		when(dmrExpander.isApplicable(any(VDomainModelReference.class))).thenReturn(EMFFormsDMRExpander.NOT_APPLICABLE);
		domainExpander.addEMFFormsDMRExpander(dmrExpander);

		domainExpander.prepareDomainObject(mock(VDomainModelReference.class), mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test
	public void testPrepareDomainObjectAllDMRExpandersConsidered() throws EMFFormsExpandingFailedException {
		final EMFFormsDMRExpander dmrExpander1 = mock(EMFFormsDMRExpander.class);
		when(dmrExpander1.isApplicable(any(VDomainModelReference.class))).thenReturn(1d);
		final EMFFormsDMRExpander dmrExpander2 = mock(EMFFormsDMRExpander.class);
		when(dmrExpander2.isApplicable(any(VDomainModelReference.class))).thenReturn(2d);
		final EMFFormsDMRExpander dmrExpander3 = mock(EMFFormsDMRExpander.class);
		when(dmrExpander3.isApplicable(any(VDomainModelReference.class))).thenReturn(3d);
		domainExpander.addEMFFormsDMRExpander(dmrExpander1);
		domainExpander.addEMFFormsDMRExpander(dmrExpander2);
		domainExpander.addEMFFormsDMRExpander(dmrExpander3);

		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		domainExpander.prepareDomainObject(dmr, mock(EObject.class));
		verify(dmrExpander1, atLeastOnce()).isApplicable(dmr);
		verify(dmrExpander2, atLeastOnce()).isApplicable(dmr);
		verify(dmrExpander3, atLeastOnce()).isApplicable(dmr);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test
	public void testPrepareDomainObjectUseCorrectDMRExpander() throws EMFFormsExpandingFailedException {
		final EMFFormsDMRExpander dmrExpander1 = mock(EMFFormsDMRExpander.class);
		when(dmrExpander1.isApplicable(any(VDomainModelReference.class))).thenReturn(1d);
		final EMFFormsDMRExpander dmrExpander2 = mock(EMFFormsDMRExpander.class);
		when(dmrExpander2.isApplicable(any(VDomainModelReference.class)))
			.thenReturn(EMFFormsDMRExpander.NOT_APPLICABLE);
		final EMFFormsDMRExpander dmrExpander3 = mock(EMFFormsDMRExpander.class);
		when(dmrExpander3.isApplicable(any(VDomainModelReference.class))).thenReturn(3d);
		domainExpander.addEMFFormsDMRExpander(dmrExpander1);
		domainExpander.addEMFFormsDMRExpander(dmrExpander2);
		domainExpander.addEMFFormsDMRExpander(dmrExpander3);

		final VDomainModelReference dmr = mock(VDomainModelReference.class);
		final EObject domainObject = mock(EObject.class);
		domainExpander.prepareDomainObject(dmr, domainObject);
		verify(dmrExpander3).prepareDomainObject(dmr, domainObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectReferenceNull() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectObjectNull() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(mock(VDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.defaultheuristic.EMFFormsDomainExpanderImpl#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectBothNull() throws EMFFormsExpandingFailedException {
		domainExpander.prepareDomainObject(null, null);
	}
}
