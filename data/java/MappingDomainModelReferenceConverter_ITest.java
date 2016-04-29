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
package org.eclipse.emfforms.internal.core.services.databinding.mapping;

import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Dictionary;
import java.util.Hashtable;

import org.eclipse.emf.databinding.IEMFValueProperty;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingDomainModelReference;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.DomainModelReferenceConverter;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

/**
 * Integration test cases for {@link MappingDomainModelReferenceConverter}.
 *
 * @author Lucas Koehler
 *
 */
public class MappingDomainModelReferenceConverter_ITest {

	private static BundleContext bundleContext;
	private DomainModelReferenceConverter service;
	private ServiceReference<DomainModelReferenceConverter> serviceReference;
	private EMFFormsDatabindingEMF emfFormsDatabinding;

	@BeforeClass
	public static void setUpBeforeClass() {
		bundleContext = FrameworkUtil.getBundle(MappingDomainModelReferenceConverter_ITest.class)
			.getBundleContext();
	}

	@Before
	public void setUp() throws DatabindingFailedException {
		final Dictionary<String, Object> dictionary = new Hashtable<String, Object>();
		dictionary.put("service.ranking", 50); //$NON-NLS-1$
		emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mock(IEMFValueProperty.class));
		bundleContext.registerService(EMFFormsDatabindingEMF.class, emfFormsDatabinding, dictionary);
		serviceReference = bundleContext
			.getServiceReference(DomainModelReferenceConverter.class);
		service = bundleContext.getService(serviceReference);
	}

	@After
	public void tearDown() {
		bundleContext.ungetService(serviceReference);
	}

	@Test
	public void testServiceType() throws DatabindingFailedException {
		assertTrue(MappingDomainModelReferenceConverter.class.isInstance(service));
		final MappingDomainModelReferenceConverter mappingConverter = (MappingDomainModelReferenceConverter) service;

		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		mappingReference.setDomainModelReference(targetReference);

		mappingConverter.convertToValueProperty(mappingReference, mock(EObject.class));
		verify(emfFormsDatabinding).getValueProperty(same(targetReference), any(EObject.class));

	}
}
