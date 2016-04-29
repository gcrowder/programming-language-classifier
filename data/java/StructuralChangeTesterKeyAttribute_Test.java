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
package org.eclipse.emfforms.internal.core.services.structuralchange.keyattribute;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyAttributeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyattributedmrFactory;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeNotification;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.eclipse.emfforms.spi.core.services.structuralchange.EMFFormsStructuralChangeTester;
import org.eclipse.emfforms.spi.core.services.structuralchange.StructuralChangeTesterInternal;
import org.junit.Before;
import org.junit.Test;

/**
 * JUnit test cases for {@link StructuralChangeTesterKeyAttribute}.
 *
 * @author Lucas Koehler
 *
 */
public class StructuralChangeTesterKeyAttribute_Test {

	private static final String WRONG_TEST_KEY2 = "WRONG_TEST_KEY2"; //$NON-NLS-1$
	private static final String WRONG_TEST_KEY = "WRONG_TEST_KEY"; //$NON-NLS-1$
	private static final String CORRECT_TEST_KEY = "CORRECT_TEST_KEY"; //$NON-NLS-1$
	private StructuralChangeTesterKeyAttribute tester;

	@Before
	public void setUp() {
		tester = new StructuralChangeTesterKeyAttribute();
	}

	@Test
	public void testIsApplicableCorrectReferenceType() {
		assertEquals(5d, tester.isApplicable(mock(VKeyAttributeDomainModelReference.class)), 0d);
	}

	@Test
	public void testIsStructureChangedKeyPresentAndValueDMRChanged() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D dWithKey = TestFactory.eINSTANCE.createD();
		dWithKey.setX(CORRECT_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(dWithKey);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(notification.getNotifier()).thenReturn(aOfC);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getA_B());

		final Setting keySetting = ((InternalEObject) dWithKey).eSetting(TestPackage.eINSTANCE.getD_X());
		when(databinding.getSetting(keyDMR, c)).thenReturn(keySetting);

		// In this case bOfC is changed => the structure of the value dmr is changed
		when(changeTester.isStructureChanged(valueDMR, c, notification)).thenReturn(true);

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertTrue(result);
	}

	@Test
	public void testIsStructureChangedKeyNotPresentAndValueDMRChanged() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D dWithoutKey = TestFactory.eINSTANCE.createD();
		dWithoutKey.setX(WRONG_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(dWithoutKey);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(notification.getNotifier()).thenReturn(aOfC);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getA_B());

		final Setting keySetting = ((InternalEObject) dWithoutKey).eSetting(TestPackage.eINSTANCE.getD_X());
		when(databinding.getSetting(keyDMR, c)).thenReturn(keySetting);

		// In this case bOfC is changed => the structure of the value dmr is changed
		when(changeTester.isStructureChanged(valueDMR, c, notification)).thenReturn(true);

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertFalse(result);
	}

	@Test
	public void testIsStructureChangedNoRelevantChange() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D d = TestFactory.eINSTANCE.createD();
		d.setX(CORRECT_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(d);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(notification.getNotifier()).thenReturn(b);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getB_C());

		final Setting keySetting = ((InternalEObject) d).eSetting(TestPackage.eINSTANCE.getD_X());
		when(databinding.getSetting(keyDMR, c)).thenReturn(keySetting);

		// In this case bOfC is not changed => the structure of the value dmr is not changed
		when(changeTester.isStructureChanged(valueDMR, c, notification)).thenReturn(false);

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertFalse(result);
	}

	@Test
	public void testIsStructureChangedKeyFromCorrectToIncorrect() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D d = TestFactory.eINSTANCE.createD();
		d.setX(WRONG_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(d);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(rawNotification.getOldValue()).thenReturn(CORRECT_TEST_KEY);
		when(rawNotification.getNewValue()).thenReturn(WRONG_TEST_KEY);
		when(notification.getNotifier()).thenReturn(d);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getD_X());

		final Setting keySetting = ((InternalEObject) d).eSetting(TestPackage.eINSTANCE.getD_X());
		when(databinding.getSetting(keyDMR, c)).thenReturn(keySetting);

		// In this case bOfC is not changed => the structure of the value dmr is not changed
		when(changeTester.isStructureChanged(valueDMR, c, notification)).thenReturn(false);

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertTrue(result);
	}

	@Test
	public void testIsStructureChangedKeyFromIncorrectToCorrect() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D d = TestFactory.eINSTANCE.createD();
		d.setX(WRONG_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(d);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(rawNotification.getOldValue()).thenReturn(WRONG_TEST_KEY);
		when(rawNotification.getNewValue()).thenReturn(CORRECT_TEST_KEY);
		when(notification.getNotifier()).thenReturn(d);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getD_X());

		final Setting keySetting = ((InternalEObject) d).eSetting(TestPackage.eINSTANCE.getD_X());
		when(databinding.getSetting(keyDMR, c)).thenReturn(keySetting);

		// In this case bOfC is not changed => the structure of the value dmr is not changed
		when(changeTester.isStructureChanged(valueDMR, c, notification)).thenReturn(false);

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertTrue(result);
	}

	@Test
	public void testIsStructureChangedKeyChangeIrrelevant() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final C c = TestFactory.eINSTANCE.createC();
		final A aOfC = TestFactory.eINSTANCE.createA();
		final B bOfC = TestFactory.eINSTANCE.createB();
		final D d = TestFactory.eINSTANCE.createD();
		d.setX(WRONG_TEST_KEY);

		a.setB(b);
		b.getCList().add(c);
		c.setD(d);
		c.setA(aOfC);
		aOfC.setB(bOfC);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(rawNotification.getOldValue()).thenReturn(WRONG_TEST_KEY);
		when(rawNotification.getNewValue()).thenReturn(WRONG_TEST_KEY2);
		when(notification.getNotifier()).thenReturn(d);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getD_X());

		final Setting keySetting = ((InternalEObject) d).eSetting(TestPackage.eINSTANCE.getD_X());
		when(databinding.getSetting(keyDMR, c)).thenReturn(keySetting);

		// In this case bOfC is not changed => the structure of the value dmr is not changed
		when(changeTester.isStructureChanged(valueDMR, c, notification)).thenReturn(false);

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertFalse(result);
	}

	@Test
	public void testIsStructureChangedFeaturePathChanged() throws DatabindingFailedException {

		// Create and configure DMRs
		final VFeaturePathDomainModelReference keyDMR = createKeyDMR();
		final VFeaturePathDomainModelReference valueDMR = createValueDMR();
		final VKeyAttributeDomainModelReference keyAttributeDMR = createKeyAttributeDMR(keyDMR, valueDMR);

		// Create domain model
		final A a = TestFactory.eINSTANCE.createA();
		final B b = TestFactory.eINSTANCE.createB();
		final B bNew = TestFactory.eINSTANCE.createB();

		a.setB(bNew);

		// Mocking
		final EMFFormsDatabindingEMF databinding = mock(EMFFormsDatabindingEMF.class);
		final EMFFormsStructuralChangeTester changeTester = mock(EMFFormsStructuralChangeTester.class);
		final ModelChangeNotification notification = mock(ModelChangeNotification.class);
		final Notification rawNotification = mock(Notification.class);

		when(rawNotification.isTouch()).thenReturn(false);
		when(rawNotification.getOldValue()).thenReturn(b);
		when(rawNotification.getNewValue()).thenReturn(bNew);
		when(notification.getNotifier()).thenReturn(a);
		when(notification.getRawNotification()).thenReturn(rawNotification);
		when(notification.getStructuralFeature()).thenReturn(TestPackage.eINSTANCE.getA_B());

		tester.setEMFFormsDatabindingEMF(databinding);
		tester.setEMFFormsStructuralChangeTester(changeTester);

		final boolean result = tester.isStructureChanged(keyAttributeDMR, a, notification);

		assertTrue(result);
	}

	/**
	 * @param keyDMR
	 * @param valueDMR
	 * @return
	 */
	private VKeyAttributeDomainModelReference createKeyAttributeDMR(final VFeaturePathDomainModelReference keyDMR,
		final VFeaturePathDomainModelReference valueDMR) {
		final VKeyAttributeDomainModelReference keyAttributeDMR = VKeyattributedmrFactory.eINSTANCE
			.createKeyAttributeDomainModelReference();

		keyAttributeDMR.setKeyValue(CORRECT_TEST_KEY);
		keyAttributeDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getA_B());
		keyAttributeDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getB_CList());

		keyAttributeDMR.setKeyDMR(keyDMR);
		keyAttributeDMR.setValueDMR(valueDMR);
		return keyAttributeDMR;
	}

	/**
	 * @return
	 */
	private VFeaturePathDomainModelReference createValueDMR() {
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_A());
		valueDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getA_B());
		return valueDMR;
	}

	/**
	 * @return
	 */
	private VFeaturePathDomainModelReference createKeyDMR() {
		final VFeaturePathDomainModelReference keyDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyDMR.getDomainModelEReferencePath().add(TestPackage.eINSTANCE.getC_D());
		keyDMR.setDomainModelEFeature(TestPackage.eINSTANCE.getD_X());
		return keyDMR;
	}

	@Test
	public void testIsApplicableWrongReferenceType() {
		assertEquals(StructuralChangeTesterInternal.NOT_APPLICABLE,
			tester.isApplicable(mock(VFeaturePathDomainModelReference.class)), 0d);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsApplicableNullReference() {
		tester.isApplicable(null);
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsStructureChangedNullReference() {
		tester.isStructureChanged(null, mock(EObject.class), mock(ModelChangeNotification.class));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsStructureChangedNullEObject() {
		tester.isStructureChanged(mock(VDomainModelReference.class), null, mock(ModelChangeNotification.class));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testIsStructureChangedNullNotification() {
		tester.isStructureChanged(mock(VDomainModelReference.class), mock(EObject.class), null);
	}
}
