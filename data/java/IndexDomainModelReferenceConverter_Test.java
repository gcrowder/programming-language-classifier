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
package org.eclipse.emfforms.internal.core.services.databinding.index;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
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
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.DomainModelReferenceConverter;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * JUnit tests for {@link IndexDomainModelReferenceConverter}.
 *
 * @author Lucas Koehler
 *
 */
public class IndexDomainModelReferenceConverter_Test {

	private IndexDomainModelReferenceConverter converter;
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
	 * Set up executed before every test case.
	 */
	@Before
	public void setUp() {
		converter = new IndexDomainModelReferenceConverter();
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#isApplicable(VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		assertEquals(10d, converter.isApplicable(mock(VIndexDomainModelReference.class)), 0d);

		// The IndexDomainModelReferenceConverter is not applicable other references than
		// IndexDomainModelReferences
		assertEquals(DomainModelReferenceConverter.NOT_APPLICABLE,
			converter.isApplicable(mock(VDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValuePropertyIndex0() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(0);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getD_X());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetDMR, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IValueProperty resultProperty = converter.convertToValueProperty(indexDMR, validEObject);

		final String expected = "A.b<B> => B.cList<C> index 0 => C.d<D> => D.x<EString>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValuePropertyIndex1() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(1);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getD_X());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetDMR, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IValueProperty resultProperty = converter.convertToValueProperty(indexDMR, validEObject);

		final String expected = "A.b<B> => B.cList<C> index 1 => C.d<D> => D.x<EString>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToValuePropertyNoReferencePath() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(1);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getD_X());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetDMR, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IValueProperty resultProperty = converter.convertToValueProperty(indexDMR, validEObject);

		final String expected = "B.cList<C> index 1 => C.d<D> => D.x<EString>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalListTypeException.class)
	public void testConvertToValuePropertyWrongListType() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		indexDMR.setIndex(1);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		indexDMR.setTargetDMR(targetDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getD_X());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(targetDMR, validEObject)).thenReturn(targetValueProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.convertToValueProperty(indexDMR, validEObject);

	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToValuePropertyNoFeature() throws DatabindingFailedException {
		final VIndexDomainModelReference indexReference = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		converter.convertToValueProperty(indexReference, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed (expected for this test case)
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyNull() throws DatabindingFailedException {
		converter.convertToValueProperty(null, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToValueProperty(VDomainModelReference,EObject)}
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
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListPropertyIndex0() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(0);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getD_YList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetDMR, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IListProperty resultProperty = converter.convertToListProperty(indexDMR, validEObject);

		final String expected = "A.b<B> => B.cList<C> index 0 => C.d<D> => D.yList[]<EInt>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListPropertyIndex1() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(1);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getD_YList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetDMR, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IListProperty resultProperty = converter.convertToListProperty(indexDMR, validEObject);

		final String expected = "A.b<B> => B.cList<C> index 1 => C.d<D> => D.yList[]<EInt>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testConvertToListPropertyNoReferencePath() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(1);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getD_YList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetDMR, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IListProperty resultProperty = converter.convertToListProperty(indexDMR, validEObject);

		final String expected = "B.cList<C> index 1 => C.d<D> => D.yList[]<EInt>"; //$NON-NLS-1$
		assertEquals(expected, resultProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalListTypeException.class)
	public void testConvertToListPropertyWrongListType() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		indexDMR.setIndex(1);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		indexDMR.setTargetDMR(targetDMR);

		final IEMFListProperty targetListProperty = EMFProperties.list(TestPackage.eINSTANCE.getD_YList());
		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(targetDMR, validEObject)).thenReturn(targetListProperty);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.convertToListProperty(indexDMR, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
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
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyWrongReferenceType() throws DatabindingFailedException {
		converter.convertToListProperty(mock(VDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.index.IndexDomainModelReferenceConverter#convertToListProperty(VDomainModelReference,EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException if the databinding failed (expected for this test case)
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToListPropertyNoFeature() throws DatabindingFailedException {
		final VIndexDomainModelReference indexReference = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		converter.convertToListProperty(indexReference, validEObject);
	}

	@Test
	public void testGetSetting() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(0);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final D d = TestFactory.eINSTANCE.createD();

		final String expected = "My Value"; //$NON-NLS-1$

		a.setB(b);
		b.getCList().add(c);
		c.setD(d);
		d.setX(expected);

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getSetting(targetDMR, c))
			.thenReturn(InternalEObject.class.cast(d).eSetting(TestPackage.eINSTANCE.getD_X()));
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final Setting setting = converter.getSetting(indexDMR, a);

		// Check value.
		assertEquals(expected, setting.get(true));
	}

	@Test
	public void testGetSettingPrefixDMR() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setIndex(0);

		final VFeaturePathDomainModelReference prefixDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		prefixDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		prefixDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setPrefixDMR(prefixDMR);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final D d = TestFactory.eINSTANCE.createD();

		final String expected = "My Value"; //$NON-NLS-1$

		a.setB(b);
		b.getCList().add(c);
		c.setD(d);
		d.setX(expected);

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getSetting(prefixDMR, a))
			.thenReturn(InternalEObject.class.cast(b).eSetting(TestPackage.eINSTANCE.getB_CList()));
		when(emfFormsDatabinding.getSetting(targetDMR, c))
			.thenReturn(InternalEObject.class.cast(d).eSetting(TestPackage.eINSTANCE.getD_X()));
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final Setting setting = converter.getSetting(indexDMR, a);

		// Check value.
		assertEquals(expected, setting.get(true));
	}

	@Test
	public void testGetSettingNoReferencePath() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());
		indexDMR.setIndex(0);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		targetDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		indexDMR.setTargetDMR(targetDMR);

		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final D d = TestFactory.eINSTANCE.createD();

		final String expected = "My Value"; //$NON-NLS-1$

		b.getCList().add(c);
		c.setD(d);
		d.setX(expected);

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getSetting(targetDMR, c))
			.thenReturn(InternalEObject.class.cast(d).eSetting(TestPackage.eINSTANCE.getD_X()));
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final Setting setting = converter.getSetting(indexDMR, b);

		// Check value.
		assertEquals(expected, setting.get(true));
	}

	@Test(expected = IllegalListTypeException.class)
	public void testGetSettingWrongListType() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());
		indexDMR.setIndex(0);

		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		indexDMR.setTargetDMR(targetDMR);

		final D d = TestFactory.eINSTANCE.createD();

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getSetting(targetDMR, d))
			.thenReturn(InternalEObject.class.cast(d).eSetting(TestPackage.eINSTANCE.getD_YList()));
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.getSetting(indexDMR, d);

	}

	@Test(expected = DatabindingFailedException.class)
	public void testGetSettingNoFeature() throws DatabindingFailedException {
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		converter.getSetting(indexDMR, validEObject);
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
