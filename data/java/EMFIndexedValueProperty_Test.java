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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.B;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFIndexedValueProperty}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFIndexedValueProperty_Test {
	/**
	 * Test method for
	 * {@link EMFIndexedValueProperty#EMFIndexedValueProperty(org.eclipse.emf.edit.domain.EditingDomain,int, org.eclipse.emf.ecore.EStructuralFeature)}
	 * .
	 */
	@Test(expected = IllegalArgumentException.class)
	public void testEMFIndexedValuePropertyNegativeIndex() {
		new EMFIndexedValueProperty(null, -1, TestPackage.eINSTANCE.getB_CList());
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doGetValue(java.lang.Object)} .
	 */
	@Test
	public void testDoGetValueNoChildrenIndex0() {
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(null, 0,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		assertNull(indexedValueProperty.doGetValue(b));
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doGetValue(java.lang.Object)} .
	 */
	@Test
	public void testDoGetValueNullNoChildrenIndex1() {
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(null, 1,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		assertNull(indexedValueProperty.doGetValue(b));
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doGetValue(java.lang.Object)} .
	 */
	@Test
	public void testDoGetValueNullOneChildIndex1() {
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(null, 1,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		final C c0 = TestFactory.eINSTANCE.createC();
		b.getCList().add(c0);
		assertNull(indexedValueProperty.doGetValue(b));
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doGetValue(java.lang.Object)} .
	 */
	@Test
	public void testDoGetValueIndex0() {
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(null, 0,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		final C c1 = TestFactory.eINSTANCE.createC();
		final C c2 = TestFactory.eINSTANCE.createC();
		b.getCList().add(c1);
		b.getCList().add(c2);

		assertEquals(c1, indexedValueProperty.doGetValue(b));
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doGetValue(java.lang.Object)} .
	 */
	@Test
	public void testDoGetValueIndex1() {
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(null, 1,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		final C c1 = TestFactory.eINSTANCE.createC();
		final C c2 = TestFactory.eINSTANCE.createC();
		b.getCList().add(c1);
		b.getCList().add(c2);

		assertEquals(c2, indexedValueProperty.doGetValue(b));
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doSetValue(java.lang.Object, java.lang.Object)} .
	 */
	@Test
	public void testDoSetValueObjectReplace() {
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(domain, 1,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		final C c1 = TestFactory.eINSTANCE.createC();
		final C c2 = TestFactory.eINSTANCE.createC();
		final C c3 = TestFactory.eINSTANCE.createC();
		b.getCList().add(c1);
		b.getCList().add(c2);
		indexedValueProperty.doSetValue(b, c3);

		assertEquals(indexedValueProperty.doGetValue(b), c3);
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doSetValue(java.lang.Object, java.lang.Object)} .
	 */
	@Test
	public void testDoSetValueObjectAdd() {
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(domain, 1,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		final C c1 = TestFactory.eINSTANCE.createC();
		final C c2 = TestFactory.eINSTANCE.createC();
		b.getCList().add(c1);
		indexedValueProperty.doSetValue(b, c2);

		assertEquals(c2, indexedValueProperty.doGetValue(b));
	}

	/**
	 * Test method for {@link EMFIndexedValueProperty#doSetValue(java.lang.Object, java.lang.Object)} .
	 */
	@Test
	public void testDoSetValueObjectIndexTooBig() {
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());
		final EMFIndexedValueProperty indexedValueProperty = new EMFIndexedValueProperty(domain, 5,
			TestPackage.eINSTANCE.getB_CList());
		final B b = TestFactory.eINSTANCE.createB();
		final C c1 = TestFactory.eINSTANCE.createC();
		final C c2 = TestFactory.eINSTANCE.createC();
		b.getCList().add(c1);
		indexedValueProperty.doSetValue(b, c2);
		assertEquals(1, b.getCList().size());
		assertTrue(b.getCList().contains(c1));
		assertFalse(b.getCList().contains(c2));
	}
}
