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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.eclipse.core.databinding.property.list.IListProperty;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.databinding.EMFProperties;
import org.eclipse.emf.databinding.IEMFListProperty;
import org.eclipse.emf.databinding.IEMFValueProperty;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingDomainModelReference;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.DomainModelReferenceConverter;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * JUnit test cases for {@link MappingDomainModelReferenceConverter}.
 *
 * @author Lucas Koehler
 *
 */
public class MappingDomainModelReferenceConverter_Test {

	private MappingDomainModelReferenceConverter converter;
	private static EObject validEObject;

	@BeforeClass
	public static void setupClass() {
		validEObject = createValidEObject();
	}

	private static EObject createValidEObject() {
		final ResourceSet rs = new ResourceSetImpl();
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack(), rs);
		rs.eAdapters().add(new AdapterFactoryEditingDomain.EditingDomainProvider(domain));
		final Resource resource = rs.createResource(URI.createURI("VIRTAUAL_URI")); //$NON-NLS-1$
		final EObject domainObject = EcoreFactory.eINSTANCE.createEObject();
		if (resource != null) {
			resource.getContents().add(domainObject);
		}
		return domainObject;
	}

	/**
	 * Creates a new {@link MappingDomainModelReferenceConverter} for every test case
	 */
	@Before
	public void setUp() {
		converter = new MappingDomainModelReferenceConverter();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#isApplicable(VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		assertEquals(10d, converter.isApplicable(mock(VMappingDomainModelReference.class)), 0d);

		assertEquals(DomainModelReferenceConverter.NOT_APPLICABLE,
			converter.isApplicable(mock(VDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValueProperty() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		mappingReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getB_C());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		mappingReference.setDomainModelReference(targetReference);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getB_C());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetReference, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IValueProperty resultProperty = converter.convertToValueProperty(mappingReference, validEObject);

		final String expected = "B.c<C> => C.eClassToA<EClassToAMap> mapping D => A.b<B> => B.c<C>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
		verify(emfFormsDatabinding).getValueProperty(targetReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValuePropertyNoReferencePaths() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());
		mappingReference.setDomainModelReference(targetReference);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetReference, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IValueProperty resultProperty = converter.convertToValueProperty(mappingReference, validEObject);

		final String expected = "C.eClassToA<EClassToAMap> mapping D => A.b<B>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
		verify(emfFormsDatabinding).getValueProperty(targetReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalMapTypeException.class)
	public void testConvertToValuePropertyWrongMapType() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToString());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());
		mappingReference.setDomainModelReference(targetReference);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetReference, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.convertToValueProperty(mappingReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyWrongReference() throws DatabindingFailedException {
		converter.convertToValueProperty(mock(VDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToValuePropertyNoFeature() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		converter.convertToValueProperty(mappingReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListProperty() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		mappingReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getB_C());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		mappingReference.setDomainModelReference(targetReference);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getB_CList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetReference, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IListProperty resultProperty = converter.convertToListProperty(mappingReference, validEObject);

		final String expected = "B.c<C> => C.eClassToA<EClassToAMap> mapping D => A.b<B> => B.cList[]<C>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
		verify(emfFormsDatabinding).getListProperty(targetReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListPropertyNoReferencePath() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		mappingReference.setDomainModelReference(targetReference);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getB_CList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetReference, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IListProperty resultProperty = converter.convertToListProperty(mappingReference, validEObject);

		final String expected = "C.eClassToA<EClassToAMap> mapping D => A.b<B> => B.cList[]<C>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
		verify(emfFormsDatabinding).getListProperty(targetReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalMapTypeException.class)
	public void testConvertToListPropertyWrongMapType() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToString());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		mappingReference.setDomainModelReference(targetReference);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getB_CList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetReference, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.convertToListProperty(mappingReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyWrongReference() throws DatabindingFailedException {
		converter.convertToListProperty(mock(VDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.MappingDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToListPropertyNoFeature() throws DatabindingFailedException {
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		converter.convertToListProperty(mappingReference, validEObject);
	}

	@Test
	public void testGetSetting() throws DatabindingFailedException {
		final DefaultRealm realm = new DefaultRealm();
		final VMappingDomainModelReference mappingReference = VMappingdmrFactory.eINSTANCE
			.createMappingDomainModelReference();
		mappingReference.setDomainModelEFeature(TestPackage.eINSTANCE.getC_EClassToA());
		mappingReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getB_C());
		mappingReference.setMappedClass(TestPackage.eINSTANCE.getD());

		final VFeaturePathDomainModelReference targetReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetReference.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		targetReference.setDomainModelEFeature(TestPackage.eINSTANCE.getB_C());
		mappingReference.setDomainModelReference(targetReference);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final B b2 = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final C c2 = TestFactory.eINSTANCE.createC();

		b.setC(c);
		c.getEClassToA().put(TestPackage.eINSTANCE.getD(), a);
		a.setB(b2);
		b2.setC(c2);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getA_B());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getB_C());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetReference, b)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final Setting setting = converter.getSetting(mappingReference, b);

		assertEquals(c2, setting.get(true));
		realm.dispose();
	}
}
