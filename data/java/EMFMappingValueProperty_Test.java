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
import static org.junit.Assert.assertNull;

import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.A;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.C;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.junit.Test;

/**
 * JUnit test cases for {@link EMFMappingValueProperty}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFMappingValueProperty_Test {
	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.EMFMappingValueProperty#doGetValue(java.lang.Object)}
	 * .
	 */
	@Test
	public void testDoGetValue() {
		final C c = TestFactory.eINSTANCE.createC();
		final EClass eClass1 = TestPackage.eINSTANCE.getB();
		final A a1 = TestFactory.eINSTANCE.createA();
		c.getEClassToA().put(eClass1, a1);

		final EMFMappingValueProperty mappingValueProperty = new EMFMappingValueProperty(null, eClass1,
			TestPackage.eINSTANCE.getC_EClassToA());
		assertEquals(a1, mappingValueProperty.doGetValue(c));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.EMFMappingValueProperty#doGetValue(java.lang.Object)}
	 * .
	 */
	@Test
	public void testDoGetValueNoMapEntry() {
		final C c = TestFactory.eINSTANCE.createC();
		final EClass eClass1 = TestPackage.eINSTANCE.getB();

		final EMFMappingValueProperty mappingValueProperty = new EMFMappingValueProperty(null, eClass1,
			TestPackage.eINSTANCE.getC_EClassToA());
		assertNull(mappingValueProperty.doGetValue(c));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.EMFMappingValueProperty#doSetValue(java.lang.Object, java.lang.Object)}
	 * .
	 */
	@Test
	public void testDoSetValueObjectReplaceEntry() {
		final C c = TestFactory.eINSTANCE.createC();
		final EClass eClass1 = TestPackage.eINSTANCE.getB();
		final A a1 = TestFactory.eINSTANCE.createA();
		final A a2 = TestFactory.eINSTANCE.createA();
		a2.setB(TestFactory.eINSTANCE.createB());
		c.getEClassToA().put(eClass1, a1);

		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());

		final EMFMappingValueProperty mappingValueProperty = new EMFMappingValueProperty(domain, eClass1,
			TestPackage.eINSTANCE.getC_EClassToA());
		assertEquals(a1, mappingValueProperty.doGetValue(c));

		mappingValueProperty.doSetValue(c, a2);
		assertEquals(a2, mappingValueProperty.doGetValue(c));
	}

	/**
	 * Test method for
	 * {@link org.eclipse.emfforms.internal.core.services.databinding.mapping.EMFMappingValueProperty#doSetValue(java.lang.Object, java.lang.Object)}
	 * .
	 */
	@Test
	public void testDoSetValueObjectAddEntry() {
		final C c = TestFactory.eINSTANCE.createC();
		final EClass eClass1 = TestPackage.eINSTANCE.getA();
		final A a1 = TestFactory.eINSTANCE.createA();
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(
			new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE),
			new BasicCommandStack());

		final EMFMappingValueProperty mappingValueProperty = new EMFMappingValueProperty(domain, eClass1,
			TestPackage.eINSTANCE.getC_EClassToA());
		mappingValueProperty.doSetValue(c, a1);

		assertEquals(a1, mappingValueProperty.doGetValue(c));
	}
}
