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
package org.eclipse.emfforms.core.services.databinding.integrationtest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.LinkedList;

import org.eclipse.core.databinding.property.list.IListProperty;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.databinding.IEMFListProperty;
import org.eclipse.emf.databinding.IEMFValueProperty;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

/**
 * JUnit test to test the integration of the databinding services.
 *
 * @author Lucas Koehler
 *
 */
public class DatabindingIntegration_ITest {

	private static BundleContext bundleContext;
	private EMFFormsDatabinding databindingService;
	private ServiceReference<EMFFormsDatabinding> serviceReference;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(DatabindingIntegration_ITest.class).getBundleContext();
	}

	@Before
	public void setUp() {
		serviceReference = bundleContext
			.getServiceReference(EMFFormsDatabinding.class);
		databindingService = bundleContext.getService(serviceReference);
	}

	@After
	public void tearDown() {
		bundleContext.ungetService(serviceReference);
	}

	@Test
	public void testIntegrationValue() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		// create reference path to the attribute
		final LinkedList<EReference> referencePath = new LinkedList<EReference>();
		referencePath.add(TestPackage.eINSTANCE.getA_B());
		referencePath.add(TestPackage.eINSTANCE.getB_C());
		referencePath.add(TestPackage.eINSTANCE.getC_D());

		final EStructuralFeature feature = TestPackage.eINSTANCE.getD_X();

		pathReference.getDomainModelEReferencePath().addAll(referencePath);
		pathReference.setDomainModelEFeature(feature);

		final IValueProperty valueProperty = databindingService.getValueProperty(pathReference, null);

		// The converter should return an IEMFValueProperty
		assertTrue(valueProperty instanceof IEMFValueProperty);

		final IEMFValueProperty emfProperty = (IEMFValueProperty) valueProperty;

		// Check EStructuralFeature of the property.
		assertEquals(feature, emfProperty.getStructuralFeature());

		// Check correct path.
		final String expected = "A.b<B> => B.c<C> => C.d<D> => D.x<EString>"; //$NON-NLS-1$
		assertEquals(expected, emfProperty.toString());

	}

	@Test
	public void testIntegrationList() throws DatabindingFailedException {
		// TODO
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		// create reference path to the attribute
		final LinkedList<EReference> referencePath = new LinkedList<EReference>();
		referencePath.add(TestPackage.eINSTANCE.getA_B());
		referencePath.add(TestPackage.eINSTANCE.getB_C());
		referencePath.add(TestPackage.eINSTANCE.getC_D());

		final EStructuralFeature feature = TestPackage.eINSTANCE.getD_YList();

		pathReference.getDomainModelEReferencePath().addAll(referencePath);
		pathReference.setDomainModelEFeature(feature);

		final IListProperty listProperty = databindingService.getListProperty(pathReference, null);

		// The converter should return an IEMFListProperty
		assertTrue(listProperty instanceof IEMFListProperty);

		final IEMFListProperty emfListProperty = (IEMFListProperty) listProperty;

		// Check EStructuralFeature of the property.
		assertEquals(feature, emfListProperty.getStructuralFeature());

		// Check correct path.
		final String expected = "A.b<B> => B.c<C> => C.d<D> => D.yList[]<EInt>"; //$NON-NLS-1$
		assertEquals(expected, emfListProperty.toString());

	}
}
