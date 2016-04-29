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
package org.eclipse.emfforms.internal.core.services.databinding.keyattribute;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyAttributeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyattributedmrFactory;
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
 * JUnit test cases for {@link KeyAttributeDomainModelReferenceConverter}.
 *
 * @author Lucas Koehler
 *
 */
public class KeyAttributeDomainModelReferenceConverter_Test {

	private static final String WRONG_TEST_KEY = "WrongTestKey"; //$NON-NLS-1$

	private static final String CORRECT_TEST_KEY = "CorrectTestKey"; //$NON-NLS-1$

	private static EObject validEObject;

	private KeyAttributeDomainModelReferenceConverter converter;

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
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		converter = new KeyAttributeDomainModelReferenceConverter();
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToValueProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testConvertToValueProperty() throws DatabindingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_A());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getA_B());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(valueDMR, validEObject)).thenReturn(targetValueProperty);

		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IEMFValueProperty resultValueProperty = converter.convertToValueProperty(keyAttributeDMR, validEObject);

		final String expected = String.format("A.b<B> => B.cList<C> key '%s' => C.a<A> => A.b<B>", CORRECT_TEST_KEY); //$NON-NLS-1$
		assertEquals(expected, resultValueProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToValueProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testConvertToValuePropertyNoReferencePath() throws DatabindingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_A());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getA_B());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(valueDMR, validEObject)).thenReturn(targetValueProperty);

		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IEMFValueProperty resultValueProperty = converter.convertToValueProperty(keyAttributeDMR, validEObject);

		final String expected = String.format("B.cList<C> key '%s' => C.a<A> => A.b<B>", CORRECT_TEST_KEY); //$NON-NLS-1$
		assertEquals(expected, resultValueProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToValueProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToValuePropertyNoFeature() throws DatabindingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_A());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getA_B());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(valueDMR, validEObject)).thenReturn(targetValueProperty);

		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.convertToValueProperty(keyAttributeDMR, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToValueProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyDMRNull() throws DatabindingFailedException {
		converter.convertToValueProperty(null, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToValueProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyObjectNull() throws DatabindingFailedException {
		converter.convertToValueProperty(mock(VKeyAttributeDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToValueProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToValuePropertyWrongReferenceType() throws DatabindingFailedException {
		converter.convertToValueProperty(mock(VFeaturePathDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToListProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testConvertToListProperty() throws DatabindingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getD_YList());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(valueDMR, validEObject)).thenReturn(targetListProperty);

		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IEMFListProperty resultListProperty = converter.convertToListProperty(keyAttributeDMR, validEObject);

		final String expected = String.format("A.b<B> => B.cList<C> key '%s' => C.d<D> => D.yList[]<EInt>", //$NON-NLS-1$
			CORRECT_TEST_KEY);
		assertEquals(expected, resultListProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToListProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testConvertToListPropertyNoReferencePath() throws DatabindingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getD_YList());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(valueDMR, validEObject)).thenReturn(targetListProperty);

		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final IEMFListProperty resultListProperty = converter.convertToListProperty(keyAttributeDMR, validEObject);

		final String expected = String.format("B.cList<C> key '%s' => C.d<D> => D.yList[]<EInt>", //$NON-NLS-1$
			CORRECT_TEST_KEY);
		assertEquals(expected, resultListProperty.toString());
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToListProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testConvertToListPropertyNoFeature() throws DatabindingFailedException {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_YList());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_D());
		final IEMFListProperty targetListProperty = targetValueProperty.list(TestPackage.eINSTANCE.getD_YList());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getListProperty(valueDMR, validEObject)).thenReturn(targetListProperty);

		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.convertToListProperty(keyAttributeDMR, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToListProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyDMRNull() throws DatabindingFailedException {
		converter.convertToListProperty(null, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToListProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyWrongReferenceType() throws DatabindingFailedException {
		converter.convertToListProperty(mock(VFeaturePathDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#convertToListProperty(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testConvertToListPropertyObjectNull() throws DatabindingFailedException {
		converter.convertToListProperty(mock(VKeyAttributeDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#getSetting(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testGetSettingKeyPresent() throws DatabindingFailedException {
		final DefaultRealm realm = new DefaultRealm();

		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithKey = TestFactory.eINSTANCE.createC();
		final D dWithKey = TestFactory.eINSTANCE.createD();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();

		a.setB(b);
		b.getCList().add(cWithKey);
		cWithKey.setD(dWithKey);
		cWithKey.setA(aOfC);
		aOfC.setB(bOfC);
		dWithKey.setX(CORRECT_TEST_KEY);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_A());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getA_B());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(valueDMR, a)).thenReturn(targetValueProperty);

		final Setting keySetting = ((InternalEObject) dWithKey).eSetting(TestPackage.eINSTANCE.getD_X());
		when(emfFormsDatabinding.getSetting(keyDMR, cWithKey)).thenReturn(keySetting);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		final Setting resultSetting = converter.getSetting(keyAttributeDMR, a);
		assertEquals(bOfC, resultSetting.get(true));

		realm.dispose();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#getSetting(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = DatabindingFailedException.class)
	public void testGetSettingKeyNotPresent() throws DatabindingFailedException {
		final DefaultRealm realm = new DefaultRealm();

		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);

		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithoutKey = TestFactory.eINSTANCE.createC();
		final D dWithoutKey = TestFactory.eINSTANCE.createD();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();

		a.setB(b);
		b.getCList().add(cWithoutKey);
		cWithoutKey.setD(dWithoutKey);
		cWithoutKey.setA(aOfC);
		aOfC.setB(bOfC);
		dWithoutKey.setX(WRONG_TEST_KEY);

		IEMFValueProperty targetValueProperty = EMFProperties.value(TestPackage.eINSTANCE.getC_A());
		targetValueProperty = targetValueProperty.value(TestPackage.eINSTANCE.getA_B());

		final EMFFormsDatabindingEMF emfFormsDatabinding = mock(EMFFormsDatabindingEMF.class);
		when(emfFormsDatabinding.getValueProperty(valueDMR, a)).thenReturn(targetValueProperty);

		final Setting keySetting = ((InternalEObject) dWithoutKey).eSetting(TestPackage.eINSTANCE.getD_X());
		when(emfFormsDatabinding.getSetting(keyDMR, cWithoutKey)).thenReturn(keySetting);
		converter.setEMFFormsDatabinding(emfFormsDatabinding);

		converter.getSetting(keyAttributeDMR, a);

		realm.dispose();
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#getSetting(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetSettingReferenceNull() throws DatabindingFailedException {
		converter.getSetting(null, validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#getSetting(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetSettingObjectNull() throws DatabindingFailedException {
		converter.getSetting(mock(VKeyAttributeDomainModelReference.class), null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#getSetting(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference, org.eclipse.emf.ecore.EObject)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testGetSettingWrongReferenceType() throws DatabindingFailedException {
		converter.getSetting(mock(VFeaturePathDomainModelReference.class), validEObject);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicable() {
		assertEquals(10d, converter.isApplicable(mock(VKeyAttributeDomainModelReference.class)), 0d);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testIsApplicableNullReference() {
		converter.isApplicable(null);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.KeyAttributeDomainModelReferenceConverter#isApplicable(org.eclipse.emf.ecp.view.spi.model.VDomainModelReference)}
	 * .
	 */
	@Test
	public void testIsApplicableWrongReferenceType() {
		assertEquals(DomainModelReferenceConverter.NOT_APPLICABLE,
			converter.isApplicable(mock(VFeaturePathDomainModelReference.class)), 0d);
	}
}
