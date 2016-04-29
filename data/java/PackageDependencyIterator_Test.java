/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.edapt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;
import java.util.Set;

import org.eclipse.emf.ecp.view.edapt.util.test.model.a.EdaptTestAPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestBPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestCPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestDPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestEPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestFPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.w.EdaptTestWPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.x.EdaptTestXPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.y.EdaptTestYPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.z.EdaptTestZPackage;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests the {@link PackageDependencyIterator}. Setup is performed via {@link PackageDependencyGraph}, so this is a
 * integration test between the two components.
 *
 * @author jfaltermeier
 *
 */
public class PackageDependencyIterator_Test {

	@BeforeClass
	public static void beforeClass() {
		// non osgi -> register epackages by hand
		EdaptTestAPackage.eINSTANCE.eClass();
		EdaptTestBPackage.eINSTANCE.eClass();
		EdaptTestCPackage.eINSTANCE.eClass();
		EdaptTestDPackage.eINSTANCE.eClass();
		EdaptTestEPackage.eINSTANCE.eClass();
		EdaptTestFPackage.eINSTANCE.eClass();
		EdaptTestWPackage.eINSTANCE.eClass();
		EdaptTestXPackage.eINSTANCE.eClass();
		EdaptTestYPackage.eINSTANCE.eClass();
		EdaptTestZPackage.eINSTANCE.eClass();
	}

	@Test
	public void testIteratorWithCircles() {
		final PackageDependencyGraph packageDependencyTree = new PackageDependencyGraph();
		packageDependencyTree.addPackage(EdaptTestFPackage.eNS_URI);
		final Iterator<Set<String>> iterator = packageDependencyTree.getIerator();

		// A
		assertTrue(iterator.hasNext());
		final Set<String> aSet = iterator.next();
		assertEquals(1, aSet.size());
		assertTrue(aSet.contains(EdaptTestAPackage.eNS_URI));

		// B C D
		assertTrue(iterator.hasNext());
		final Set<String> bcdSet = iterator.next();
		assertEquals(3, bcdSet.size());
		assertTrue(bcdSet.contains(EdaptTestBPackage.eNS_URI));
		assertTrue(bcdSet.contains(EdaptTestCPackage.eNS_URI));
		assertTrue(bcdSet.contains(EdaptTestDPackage.eNS_URI));

		// E F
		assertTrue(iterator.hasNext());
		final Set<String> efSet = iterator.next();
		assertEquals(2, efSet.size());
		assertTrue(efSet.contains(EdaptTestEPackage.eNS_URI));
		assertTrue(efSet.contains(EdaptTestFPackage.eNS_URI));
		assertFalse(iterator.hasNext());
	}

	@Test
	public void testIteratorWithCircleAtStart() {
		final PackageDependencyGraph packageDependencyTree = new PackageDependencyGraph();
		packageDependencyTree.addPackage(EdaptTestZPackage.eNS_URI);
		final Iterator<Set<String>> iterator = packageDependencyTree.getIerator();

		// W X
		assertTrue(iterator.hasNext());
		final Set<String> wxSet = iterator.next();
		assertEquals(2, wxSet.size());
		assertTrue(wxSet.contains(EdaptTestWPackage.eNS_URI));
		assertTrue(wxSet.contains(EdaptTestXPackage.eNS_URI));

		// Y Z
		assertTrue(iterator.hasNext());
		final Set<String> yzSet = iterator.next();
		assertEquals(2, yzSet.size());
		assertTrue(yzSet.contains(EdaptTestYPackage.eNS_URI));
		assertTrue(yzSet.contains(EdaptTestZPackage.eNS_URI));
	}

}
