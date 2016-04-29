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
package org.eclipse.emfforms.internal.core.services.databinding.featurepath;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.util.LinkedList;

import org.eclipse.core.databinding.property.list.IListProperty;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.databinding.IEMFListProperty;
import org.eclipse.emf.databinding.IEMFValueProperty;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.DomainModelReferenceConverter;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * JUnit test for {@link FeaturePathDomainModelReferenceConverter}.
 *
 * @author Lucas Koehler
 *
 */
public class FeaturePathDomainModelReferenceConverter_Test {

	private FeaturePathDomainModelReferenceConverter converter;
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
	 * Set up that is executed before every test.
	 */
	@Before
	public void setUp() {
		converter = new FeaturePathDomainModelReferenceConverter();

	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		// The FeaturePathDomainModelReferenceConverter is the standard converter for VFeaturePathDomainModelReference
		// with a low priority.
		assertEquals(0.0, converter.isApplicable(mock(VFeaturePathDomainModelReference.class)), 0d);

		// The FeaturePathDomainModelReferenceConverter is not applicable other references than
		// VFeaturePathDomainModelReferences
		assertEquals(DomainModelReferenceConverter.NOT_APPLICABLE,
			converter.isApplicable(mock(VDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testIsApplicableNull() {
		converter.isApplicable(null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValueProperty() throws DatabindingFailedException {
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

		final IValueProperty valueProperty = converter.convertToValueProperty(pathReference, validEObject);

		// The converter should return an IEMFValueProperty
		assertTrue(valueProperty instanceof IEMFValueProperty);

		final IEMFValueProperty emfProperty = (IEMFValueProperty) valueProperty;

		// Check EStructuralFeature of the property.
		assertEquals(feature, emfProperty.getStructuralFeature());

		// Check correct path.
		final String expected = "A.b<B> => B.c<C> => C.d<D> => D.x<EString>"; //$NON-NLS-1$
		assertEquals(expected, emfProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValuePropertyNoReferencePath() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		final EStructuralFeature feature = TestPackage.eINSTANCE.getD_X();
		pathReference.setDomainModelEFeature(feature);

		final IValueProperty valueProperty = converter.convertToValueProperty(pathReference, validEObject);

		// The converter should return an IEMFValueProperty
		assertTrue(valueProperty instanceof IEMFValueProperty);

		final IEMFValueProperty emfProperty = (IEMFValueProperty) valueProperty;

		// Check EStructuralFeature of the property.
		assertEquals(feature, emfProperty.getStructuralFeature());

		// Check correct path.
		final String expected = "D.x<EString>"; //$NON-NLS-1$
		assertEquals(expected, emfProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToValuePropertyNoFeature() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		converter.convertToValueProperty(pathReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyNull() throws DatabindingFailedException {
		converter.convertToValueProperty(null, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyWrongReferenceType() throws DatabindingFailedException {
		converter.convertToValueProperty(mock(VDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListProperty() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		// create reference path to the list
		final LinkedList<EReference> referencePath = new LinkedList<EReference>();
		referencePath.add(TestPackage.eINSTANCE.getA_B());
		referencePath.add(TestPackage.eINSTANCE.getB_C());
		referencePath.add(TestPackage.eINSTANCE.getC_D());

		final EStructuralFeature feature = TestPackage.eINSTANCE.getD_YList();

		pathReference.getDomainModelEReferencePath().addAll(referencePath);
		pathReference.setDomainModelEFeature(feature);

		final IListProperty listProperty = converter.convertToListProperty(pathReference, validEObject);

		// The converter should return an IEMFListProperty
		assertTrue(listProperty instanceof IEMFListProperty);

		final IEMFListProperty emfProperty = (IEMFListProperty) listProperty;

		// Check EStructuralFeature of the property.
		assertEquals(feature, emfProperty.getStructuralFeature());

		// Check correct path.
		final String expected = "A.b<B> => B.c<C> => C.d<D> => D.yList[]<EInt>"; //$NON-NLS-1$
		assertEquals(expected, emfProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToListPropertyNoFeature() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		converter.convertToListProperty(pathReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListPropertyNoReferencePath() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		final EStructuralFeature feature = TestPackage.eINSTANCE.getD_YList();
		pathReference.setDomainModelEFeature(feature);

		final IListProperty listProperty = converter.convertToListProperty(pathReference, validEObject);

		// The converter should return an IEMFListProperty
		assertTrue(listProperty instanceof IEMFListProperty);

		final IEMFListProperty emfProperty = (IEMFListProperty) listProperty;

		// Check EStructuralFeature of the property.
		assertEquals(feature, emfProperty.getStructuralFeature());

		// Check correct path.
		final String expected = "D.yList[]<EInt>"; //$NON-NLS-1$
		assertEquals(expected, emfProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyNull() throws DatabindingFailedException {
		converter.convertToListProperty(null, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.core.services.databinding.featurepath.FeaturePathDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyWrongReferenceType() throws DatabindingFailedException {
		converter.convertToListProperty(mock(VDomainModelReference.class), validEObject);
	}

	@Test
	public void testGetSetting() throws DatabindingFailedException {
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

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final D d = TestFactory.eINSTANCE.createD();

		final String expected = "My Value"; //$NON-NLS-1$

		a.setB(b);
		b.setC(c);
		c.setD(d);
		d.setX(expected);

		final Setting setting = converter.getSetting(pathReference, a);

		// Check value.
		assertEquals(expected, setting.get(true));
	}

	@Test
	public void testGetSettingNoReferencePath() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		final EStructuralFeature feature = TestPackage.eINSTANCE.getD_X();
		pathReference.setDomainModelEFeature(feature);

		final D d = TestFactory.eINSTANCE.createD();
		final String expected = "My Value"; //$NON-NLS-1$
		d.setX(expected);

		final Setting setting = converter.getSetting(pathReference, d);

		// Check value.
		assertEquals(expected, setting.get(true));
	}

	@Test(expected = DatabindingFailedException.class)
	public void testGetSettingNoFeature() throws DatabindingFailedException {
		final VFeaturePathDomainModelReference pathReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		converter.getSetting(pathReference, validEObject);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetSettingNull() throws DatabindingFailedException {
		converter.getSetting(null, validEObject);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testGetSettingWrongReferenceType() throws DatabindingFailedException {
		converter.getSetting(mock(VDomainModelReference.class), validEObject);
	}
}
