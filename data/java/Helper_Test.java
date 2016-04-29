/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Alexandra Buzila - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.view.editor.controls.test;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecp.view.spi.editor.controls.Helper;
import org.junit.Test;

public class Helper_Test {

	@Test
	public void testReferenceMapReferenceCycles() {

		final EClass classA = EcoreFactory.eINSTANCE.createEClass();
		final EClass classB = EcoreFactory.eINSTANCE.createEClass();

		final EReference b1 = EcoreFactory.eINSTANCE.createEReference();
		b1.setEType(classB);
		b1.setContainment(true);
		final EReference b2 = EcoreFactory.eINSTANCE.createEReference();
		b2.setEType(classB);
		b2.setContainment(true);
		classA.getEStructuralFeatures().add(b1);
		classA.getEStructuralFeatures().add(b2);

		final EReference a1 = EcoreFactory.eINSTANCE.createEReference();
		a1.setEType(classA);
		a1.setContainment(true);
		final EReference a2 = EcoreFactory.eINSTANCE.createEReference();
		a2.setEType(classA);
		a2.setContainment(true);
		classB.getEStructuralFeatures().add(a1);
		classB.getEStructuralFeatures().add(a2);

		final HashMap<EClass, EReference> childParentReferenceMap = new HashMap<EClass, EReference>();
		Helper.getReferenceMap(classA, childParentReferenceMap);

		final EReference refA = childParentReferenceMap.get(classA);
		final EReference refB = childParentReferenceMap.get(classB);

		assertNotNull("Reference map does not contain reference of type A.", refA);
		assertNotNull("Reference map does not contain reference of type B.", refB);
		assertTrue("Reference map contains reference of wrong type (refA should be of type classA).",
			refA.getEType().equals(classA));
		assertTrue("Reference map contains reference of wrong type (refB should be of type classB).",
			refB.getEType().equals(classB));

	}

	@Test
	public void testReferenceMapNonContainmentReference() {

		final EClass classA = EcoreFactory.eINSTANCE.createEClass();
		final EClass classB = EcoreFactory.eINSTANCE.createEClass();
		final EClass classC = EcoreFactory.eINSTANCE.createEClass();

		final EReference b = EcoreFactory.eINSTANCE.createEReference();
		b.setEType(classB);
		b.setContainment(true);

		final EReference c = EcoreFactory.eINSTANCE.createEReference();
		c.setEType(classC);
		c.setContainment(false);

		classA.getEStructuralFeatures().add(b);
		classA.getEStructuralFeatures().add(c);

		final HashMap<EClass, EReference> childParentReferenceMap = new HashMap<EClass, EReference>();
		Helper.getReferenceMap(classA, childParentReferenceMap);

		final EReference refB = childParentReferenceMap.get(classB);
		final EReference refC = childParentReferenceMap.get(classC);

		assertNotNull("Reference map does not contain reference of type B.", refB);
		assertTrue("Reference map contains reference of wrong type (refA should be of type classA).",
			refB.getEType().equals(classB));
		assertNull("Reference map should not contain reference of type C (non containment reference).", refC);

	}

	@Test
	public void testReferenceMapSelfReference() {

		final EClass classA = EcoreFactory.eINSTANCE.createEClass();

		final EReference a1 = EcoreFactory.eINSTANCE.createEReference();
		a1.setEType(classA);
		a1.setContainment(true);

		classA.getEStructuralFeatures().add(a1);

		final HashMap<EClass, EReference> childParentReferenceMap = new HashMap<EClass, EReference>();
		Helper.getReferenceMap(classA, childParentReferenceMap);

		final EReference refA = childParentReferenceMap.get(classA);

		assertNull("Reference map should not contain reference of type A (self reference).", refA);

	}

}
