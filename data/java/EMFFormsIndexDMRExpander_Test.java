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
package org.eclipse.emfforms.internal.core.services.domainexpander.index;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.common.report.AbstractReport;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDMRExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsDomainExpander;
import org.eclipse.emfforms.spi.core.services.domainexpander.EMFFormsExpandingFailedException;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit tests for {@link EMFFormsIndexDMRExpander}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFFormsIndexDMRExpander_Test {

	private EMFFormsIndexDMRExpander indexDMRExpander;
	private ReportService reportService;
	private EMFFormsDomainExpander domainExpander;
	private EMFFormsDatabinding databindingService;

	/**
	 * Creates a new {@link EMFFormsIndexDMRExpander} and mocks its required services for every test case.
	 */
	@Before
	public void setUp() {
		indexDMRExpander = new EMFFormsIndexDMRExpander();
		reportService = mock(ReportService.class);
		domainExpander = mock(EMFFormsDomainExpander.class);
		databindingService = mock(EMFFormsDatabinding.class);
		indexDMRExpander.setReportService(reportService);
		indexDMRExpander.setEMFFormsDomainExpander(domainExpander);
		indexDMRExpander.setEMFFormsDatabinding(databindingService);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectReferenceNull() throws EMFFormsExpandingFailedException {
		indexDMRExpander.prepareDomainObject(null, mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectObjectNull() throws EMFFormsExpandingFailedException {
		indexDMRExpander.prepareDomainObject(mock(VIndexDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectBothNull() throws EMFFormsExpandingFailedException {
		indexDMRExpander.prepareDomainObject(null, null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testPrepareDomainObjectWrongReferenceType() throws EMFFormsExpandingFailedException {
		indexDMRExpander.prepareDomainObject(mock(VFeaturePathDomainModelReference.class), mock(EObject.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testPrepareDomainObjectWithPrefixDMRIndex0() throws EMFFormsExpandingFailedException,
		DatabindingFailedException {
		/*
		 * Note: Testing in JUnit tests is afaik only possible without using domain model e reference paths because
		 * they get expanded by the EMFFormsDomainExpander that has to be mocked in a JUnit test.
		 */

		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		final VFeaturePathDomainModelReference prefixDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		prefixDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		indexDMR.setPrefixDMR(prefixDMR);
		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(0);

		final B domainObject = TestFactory.eINSTANCE.createB();

		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(TestPackage.eINSTANCE.getB_CList());
		when(valueProperty.getValue(domainObject)).thenReturn(domainObject.getCList());

		when(databindingService.getValueProperty(any(VDomainModelReference.class), same(domainObject))).thenReturn(
			valueProperty);

		assertTrue(domainObject.getCList().size() == 0);

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
		assertTrue(domainObject.getCList().size() == 1);
		assertNotNull(domainObject.getCList().get(0));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testPrepareDomainObjectWithPrefixDMRIndex1() throws EMFFormsExpandingFailedException,
		DatabindingFailedException {
		/*
		 * Note: Testing in JUnit tests is afaik only possible without using domain model e reference paths because
		 * they get expanded by the EMFFormsDomainExpander that has to be mocked in a JUnit test.
		 */

		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		final VFeaturePathDomainModelReference prefixDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		prefixDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		indexDMR.setPrefixDMR(prefixDMR);
		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(1);

		final B domainObject = TestFactory.eINSTANCE.createB();

		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(TestPackage.eINSTANCE.getB_CList());
		when(valueProperty.getValue(domainObject)).thenReturn(domainObject.getCList());

		when(databindingService.getValueProperty(any(VDomainModelReference.class), same(domainObject))).thenReturn(
			valueProperty);

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
		assertTrue(domainObject.getCList().size() == 2);
		assertNotNull(domainObject.getCList().get(0));
		assertNotNull(domainObject.getCList().get(1));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * Tests whether the correct exception is thrown if the domain model e feature of the prefix dmr does not describe a
	 * list.
	 *
	 * @throws EMFFormsExpandingFailedException
	 * @throws DatabindingFailedException
	 */
	@Test(expected = EMFFormsExpandingFailedException.class)
	public void testPrepareDomainObjectNoListWithPrefixDMR() throws EMFFormsExpandingFailedException,
		DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		final VFeaturePathDomainModelReference prefixDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		prefixDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getC_D());

		indexDMR.setPrefixDMR(prefixDMR);
		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(1);

		final B domainObject = TestFactory.eINSTANCE.createB();

		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(TestPackage.eINSTANCE.getB_C());
		when(valueProperty.getValue(domainObject)).thenReturn(domainObject.getC());

		when(databindingService.getValueProperty(any(VDomainModelReference.class), same(domainObject))).thenReturn(
			valueProperty);

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * Tests whether the correct exception is thrown if the domain model e feature of the index dmr does not describe a
	 * list.
	 *
	 * @throws EMFFormsExpandingFailedException
	 * @throws DatabindingFailedException
	 */
	@Test(expected = EMFFormsExpandingFailedException.class)
	public void testPrepareDomainObjectNoListWithoutPrefixDMR() throws EMFFormsExpandingFailedException,
		DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getC_D());

		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(1);

		final B domainObject = TestFactory.eINSTANCE.createB();

		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(TestPackage.eINSTANCE.getB_C());
		when(valueProperty.getValue(domainObject)).thenReturn(domainObject.getC());

		when(databindingService.getValueProperty(any(VDomainModelReference.class), same(domainObject))).thenReturn(
			valueProperty);

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testPrepareDomainObjectWithoutPrefixDMRIndex0() throws EMFFormsExpandingFailedException,
		DatabindingFailedException {
		/*
		 * Note: Testing in JUnit tests is afaik know only possible without using domain model e reference paths because
		 * they get expanded by the EMFFormsDomainExpander that has to be mocked in a JUnit test.
		 */

		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		// indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		// targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(0);

		final B domainObject = TestFactory.eINSTANCE.createB();

		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(TestPackage.eINSTANCE.getB_CList());
		when(valueProperty.getValue(domainObject)).thenReturn(domainObject.getCList());

		when(databindingService.getValueProperty(any(VDomainModelReference.class), same(domainObject))).thenReturn(
			valueProperty);

		assertTrue(domainObject.getCList().size() == 0);

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
		// assertNotNull(domainObject.getB());
		assertTrue(domainObject.getCList().size() == 1);
		assertNotNull(domainObject.getCList().get(0));
		// assertNotNull(domainObject.getCList().get(1).getD());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#prepareDomainObject(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws EMFFormsExpandingFailedException
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testPrepareDomainObjectWithoutPrefixDMRIndex1() throws EMFFormsExpandingFailedException,
		DatabindingFailedException {
		/*
		 * Note: Testing in JUnit tests is afaik know only possible without using domain model e reference paths because
		 * they get expanded by the EMFFormsDomainExpander that has to be mocked in a JUnit test.
		 */

		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		// indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		// targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		indexDMR.setTargetDMR(targetDMR);
		indexDMR.setIndex(1);

		final B domainObject = TestFactory.eINSTANCE.createB();

		final IValueProperty valueProperty = mock(IValueProperty.class);
		when(valueProperty.getValueType()).thenReturn(TestPackage.eINSTANCE.getB_CList());
		when(valueProperty.getValue(domainObject)).thenReturn(domainObject.getCList());

		when(databindingService.getValueProperty(any(VDomainModelReference.class), same(domainObject))).thenReturn(
			valueProperty);

		assertTrue(domainObject.getCList().size() == 0);

		indexDMRExpander.prepareDomainObject(indexDMR, domainObject);
		// assertNotNull(domainObject.getB());
		assertTrue(domainObject.getCList().size() == 2);
		assertNotNull(domainObject.getCList().get(0));
		assertNotNull(domainObject.getCList().get(1));
		// assertNotNull(domainObject.getCList().get(1).getD());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		assertEquals(5d, indexDMRExpander.isApplicable(mock(VIndexDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableNull() {
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE, indexDMRExpander.isApplicable(null), 0d);
		verify(reportService).report(any(AbstractReport.class));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.domainexpander.index.EMFFormsIndexDMRExpander#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test()
	public void testIsApplicableWrongReferenceType() {
		assertEquals(EMFFormsDMRExpander.NOT_APPLICABLE,
			indexDMRExpander.isApplicable(mock(VFeaturePathDomainModelReference.class)), 0d);
	}
}
