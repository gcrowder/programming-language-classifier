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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.domain.EditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFKeyAttributeValueProperty}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFKeyAttributeValueProperty_Test {

	private static final String WRONG_TEST_KEY = "WrongTestKey"; //$NON-NLS-1$
	private static final String CORRECT_TEST_KEY = "CorrectTestKey"; //$NON-NLS-1$

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doGetValue(java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testDoGetValueKeyPresent() throws DatabindingFailedException {
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithoutKey = TestFactory.eINSTANCE.createC();
		final C cWithKey = TestFactory.eINSTANCE.createC();
		final D dWithKey = TestFactory.eINSTANCE.createD();
		final D dWithoutKey = TestFactory.eINSTANCE.createD();

		dWithKey.setX(CORRECT_TEST_KEY);
		dWithoutKey.setX(WRONG_TEST_KEY);

		b.getCList().add(cWithoutKey);
		b.getCList().add(cWithKey);
		cWithKey.setD(dWithKey);
		cWithoutKey.setD(dWithoutKey);

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final Setting settingWithKey = ((InternalEObject) dWithKey).eSetting(TestPackage.eINSTANCE.getD_X());
		final Setting settingWithoutKey = ((InternalEObject) dWithoutKey).eSetting(TestPackage.eINSTANCE.getD_X());

		when(databinding.getSetting(keyDMR, cWithKey)).thenReturn(settingWithKey);
		when(databinding.getSetting(keyDMR, cWithoutKey)).thenReturn(settingWithoutKey);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(mock(EditingDomain.class),
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		assertEquals(cWithKey, valueProperty.getValue(b));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doGetValue(java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testDoGetValueKeyNotPresent() throws DatabindingFailedException {
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithoutKey = TestFactory.eINSTANCE.createC();
		final D dWithoutKey = TestFactory.eINSTANCE.createD();

		dWithoutKey.setX(WRONG_TEST_KEY);

		b.getCList().add(cWithoutKey);
		cWithoutKey.setD(dWithoutKey);

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final Setting settingWithoutKey = ((InternalEObject) dWithoutKey).eSetting(TestPackage.eINSTANCE.getD_X());

		when(databinding.getSetting(keyDMR, cWithoutKey)).thenReturn(settingWithoutKey);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(mock(EditingDomain.class),
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		assertEquals(null, valueProperty.getValue(b));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doGetValue(java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testDoGetValueEmptyList() throws DatabindingFailedException {
		final B b = TestFactory.eINSTANCE.createB();

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(mock(EditingDomain.class),
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		assertEquals(null, valueProperty.getValue(b));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doGetValue(java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testDoGetValueEObjectKeyPresent() throws DatabindingFailedException {
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithoutKey = TestFactory.eINSTANCE.createC();
		final C cWithKey = TestFactory.eINSTANCE.createC();
		final D dKey = TestFactory.eINSTANCE.createD();
		final D dNoKey = TestFactory.eINSTANCE.createD();

		b.getCList().add(cWithoutKey);
		b.getCList().add(cWithKey);
		cWithKey.setD(dKey);
		cWithoutKey.setD(dNoKey);

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getC_D());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final Setting settingWithKey = ((InternalEObject) cWithKey).eSetting(TestPackage.eINSTANCE.getC_D());
		final Setting settingWithoutKey = ((InternalEObject) cWithoutKey).eSetting(TestPackage.eINSTANCE.getC_D());

		when(databinding.getSetting(keyDMR, cWithKey)).thenReturn(settingWithKey);
		when(databinding.getSetting(keyDMR, cWithoutKey)).thenReturn(settingWithoutKey);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(mock(EditingDomain.class),
			databinding, keyDMR, dKey, TestPackage.eINSTANCE.getB_CList());

		assertEquals(cWithKey, valueProperty.getValue(b));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doSetValue(java.lang.Object, java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testDoSetValueNewValueDoesntContainKey() throws DatabindingFailedException {
		final B b = TestFactory.eINSTANCE.createB();
		final C cWithoutKey = TestFactory.eINSTANCE.createC();
		final D dWithoutKey = TestFactory.eINSTANCE.createD();

		dWithoutKey.setX(WRONG_TEST_KEY);

		cWithoutKey.setD(dWithoutKey);

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final Setting settingWithoutKey = ((InternalEObject) dWithoutKey).eSetting(TestPackage.eINSTANCE.getD_X());

		when(databinding.getSetting(keyDMR, cWithoutKey)).thenReturn(settingWithoutKey);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(mock(EditingDomain.class),
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		valueProperty.setValue(b, cWithoutKey);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doSetValue(java.lang.Object, java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@SuppressWarnings("unchecked")
	@Test(expected = IllegalArgumentException.class)
	public void testDoSetValueNewValueDatabindingFails() throws DatabindingFailedException {
		final B b = TestFactory.eINSTANCE.createB();
		final C cFailedDatabinding = TestFactory.eINSTANCE.createC();

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);

		when(databinding.getSetting(keyDMR, cFailedDatabinding)).thenThrow(DatabindingFailedException.class);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(mock(EditingDomain.class),
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		valueProperty.setValue(b, cFailedDatabinding);
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doSetValue(java.lang.Object, java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testDoSetValueNoOldKeyPresent() throws DatabindingFailedException {
		final AdapterFactoryEditingDomain editingDomain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());

		final B b = TestFactory.eINSTANCE.createB();
		final C cOldWithoutKey = TestFactory.eINSTANCE.createC();
		final C cNewWithKey = TestFactory.eINSTANCE.createC();
		final D dWithKey = TestFactory.eINSTANCE.createD();
		final D dWithoutKey = TestFactory.eINSTANCE.createD();

		dWithKey.setX(CORRECT_TEST_KEY);
		dWithoutKey.setX(WRONG_TEST_KEY);

		b.getCList().add(cOldWithoutKey);
		cNewWithKey.setD(dWithKey);
		cOldWithoutKey.setD(dWithoutKey);

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final Setting settingWithKey = ((InternalEObject) dWithKey).eSetting(TestPackage.eINSTANCE.getD_X());
		final Setting settingWithoutKey = ((InternalEObject) dWithoutKey).eSetting(TestPackage.eINSTANCE.getD_X());

		when(databinding.getSetting(keyDMR, cNewWithKey)).thenReturn(settingWithKey);
		when(databinding.getSetting(keyDMR, cOldWithoutKey)).thenReturn(settingWithoutKey);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(editingDomain,
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		valueProperty.doSetValue(b, cNewWithKey);

		assertEquals(2, b.getCList().size());
		assertTrue(b.getCList().contains(cOldWithoutKey));
		assertTrue(b.getCList().contains(cNewWithKey));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.keyattribute.EMFKeyAttributeValueProperty#doSetValue(java.lang.Object, java.lang.Object)}
	 * .
	 *
	 * @throws DatabindingFailedException
	 */
	@Test
	public void testDoSetValueOldKeyPresent() throws DatabindingFailedException {
		final AdapterFactoryEditingDomain editingDomain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());

		final B b = TestFactory.eINSTANCE.createB();
		final C cOldWithKey = TestFactory.eINSTANCE.createC();
		final C cNewWithKey = TestFactory.eINSTANCE.createC();
		final D dOldWithKey = TestFactory.eINSTANCE.createD();
		final D dNewWithKey = TestFactory.eINSTANCE.createD();

		dOldWithKey.setX(CORRECT_TEST_KEY);
		dNewWithKey.setX(CORRECT_TEST_KEY);

		b.getCList().add(cOldWithKey);
		cNewWithKey.setD(dNewWithKey);
		cOldWithKey.setD(dOldWithKey);

		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());

		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final Setting settingNewWithKey = ((InternalEObject) dNewWithKey).eSetting(TestPackage.eINSTANCE.getD_X());
		final Setting settingOldWithKey = ((InternalEObject) dOldWithKey).eSetting(TestPackage.eINSTANCE.getD_X());

		when(databinding.getSetting(keyDMR, cNewWithKey)).thenReturn(settingNewWithKey);
		when(databinding.getSetting(keyDMR, cOldWithKey)).thenReturn(settingOldWithKey);

		final EMFKeyAttributeValueProperty valueProperty = new EMFKeyAttributeValueProperty(editingDomain,
			databinding, keyDMR, CORRECT_TEST_KEY, TestPackage.eINSTANCE.getB_CList());

		valueProperty.doSetValue(b, cNewWithKey);

		assertEquals(1, b.getCList().size());
		assertFalse(b.getCList().contains(cOldWithKey));
		assertTrue(b.getCList().contains(cNewWithKey));
	}
}
