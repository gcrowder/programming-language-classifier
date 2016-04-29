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
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecp.view.edapt.PackageDependencyGraph.PackageTreeNode;
import org.eclipse.emf.ecp.view.edapt.util.test.model.a.EdaptTestAPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.b.EdaptTestBPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.c.EdaptTestCPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.d.EdaptTestDPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.e.EdaptTestEPackage;
import org.eclipse.emf.ecp.view.edapt.util.test.model.f.EdaptTestFPackage;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * Tests for {@link PackageDependencyGraph}.
 *
 * @author jfaltermeier
 *
 */
public class PackageDependencyGraph_Test {

	@BeforeClass
	public static void beforeClass() {
		// non osgi -> register epackages by hand
		EdaptTestAPackage.eINSTANCE.eClass();
		EdaptTestBPackage.eINSTANCE.eClass();
		EdaptTestCPackage.eINSTANCE.eClass();
		EdaptTestDPackage.eINSTANCE.eClass();
		EdaptTestEPackage.eINSTANCE.eClass();
		EdaptTestFPackage.eINSTANCE.eClass();
	}

	@Test
	public void testRegisterNodesDependenciesRegistered() {
		final PackageDependencyGraph dependencyTree = new PackageDependencyGraph();

		// add A
		dependencyTree.addPackage(EdaptTestAPackage.eNS_URI);
		final Map<String, PackageTreeNode> aMap = dependencyTree.getNsURIToNodeMap();
		assertEquals(1, aMap.size());
		assertTrue(aMap.containsKey(EdaptTestAPackage.eNS_URI));

		// add B
		dependencyTree.addPackage(EdaptTestBPackage.eNS_URI);
		final Map<String, PackageTreeNode> abcdMap = dependencyTree.getNsURIToNodeMap();
		assertEquals(4, abcdMap.size());
		assertTrue(abcdMap.containsKey(EdaptTestAPackage.eNS_URI));
		assertTrue(abcdMap.containsKey(EdaptTestBPackage.eNS_URI));
		assertTrue(abcdMap.containsKey(EdaptTestCPackage.eNS_URI));
		assertTrue(abcdMap.containsKey(EdaptTestDPackage.eNS_URI));

		// add E
		dependencyTree.addPackage(EdaptTestEPackage.eNS_URI);
		final Map<String, PackageTreeNode> abcdefMap = dependencyTree.getNsURIToNodeMap();
		assertEquals(6, abcdefMap.size());
		assertTrue(abcdefMap.containsKey(EdaptTestAPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestBPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestCPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestDPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestEPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestFPackage.eNS_URI));
	}

	@Test
	public void testRegisterNodesDependenciesRegisteredBottom() {
		final PackageDependencyGraph dependencyTree = new PackageDependencyGraph();

		// add F
		dependencyTree.addPackage(EdaptTestFPackage.eNS_URI);
		final Map<String, PackageTreeNode> abcdefMap = dependencyTree.getNsURIToNodeMap();
		assertEquals(6, abcdefMap.size());
		assertTrue(abcdefMap.containsKey(EdaptTestAPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestBPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestCPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestDPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestEPackage.eNS_URI));
		assertTrue(abcdefMap.containsKey(EdaptTestFPackage.eNS_URI));
	}

	@Test
	public void testRegisterNodesCheckNodes() {
		final PackageDependencyGraph dependencyTree = new PackageDependencyGraph();

		// add F
		dependencyTree.addPackage(EdaptTestFPackage.eNS_URI);
		final Map<String, PackageTreeNode> map = dependencyTree.getNsURIToNodeMap();

		final PackageTreeNode aNode = map.get(EdaptTestAPackage.eNS_URI);
		assertEquals(0, aNode.getParents().size());
		assertEquals(1, aNode.getChildren().size());
		assertEquals(EdaptTestBPackage.eNS_URI, aNode.getChildren().iterator().next().getNSURI());

		final PackageTreeNode bNode = map.get(EdaptTestBPackage.eNS_URI);
		assertEquals(2, bNode.getParents().size());
		assertNodes(bNode.getParents(), EdaptTestAPackage.eNS_URI, EdaptTestCPackage.eNS_URI);
		assertEquals(1, bNode.getChildren().size());
		assertEquals(EdaptTestDPackage.eNS_URI, bNode.getChildren().iterator().next().getNSURI());

		final PackageTreeNode cNode = map.get(EdaptTestCPackage.eNS_URI);
		assertEquals(1, cNode.getParents().size());
		assertEquals(EdaptTestDPackage.eNS_URI, cNode.getParents().iterator().next().getNSURI());
		assertEquals(1, cNode.getChildren().size());
		assertEquals(EdaptTestBPackage.eNS_URI, cNode.getChildren().iterator().next().getNSURI());

		final PackageTreeNode dNode = map.get(EdaptTestDPackage.eNS_URI);
		assertEquals(1, dNode.getParents().size());
		assertEquals(EdaptTestBPackage.eNS_URI, dNode.getParents().iterator().next().getNSURI());
		assertEquals(2, dNode.getChildren().size());
		assertNodes(dNode.getChildren(), EdaptTestCPackage.eNS_URI, EdaptTestEPackage.eNS_URI);

		final PackageTreeNode eNode = map.get(EdaptTestEPackage.eNS_URI);
		assertEquals(2, eNode.getParents().size());
		assertNodes(eNode.getParents(), EdaptTestDPackage.eNS_URI, EdaptTestFPackage.eNS_URI);
		assertEquals(1, eNode.getChildren().size());
		assertEquals(EdaptTestFPackage.eNS_URI, eNode.getChildren().iterator().next().getNSURI());

		final PackageTreeNode fNode = map.get(EdaptTestFPackage.eNS_URI);
		assertEquals(1, fNode.getParents().size());
		assertEquals(EdaptTestEPackage.eNS_URI, fNode.getParents().iterator().next().getNSURI());
		assertEquals(1, fNode.getChildren().size());
		assertEquals(EdaptTestEPackage.eNS_URI, fNode.getChildren().iterator().next().getNSURI());

	}

	private static void assertNodes(Set<PackageTreeNode> actual, String... expected) {
		final Set<String> expectedNsURIs = new LinkedHashSet<String>(Arrays.asList(expected));
		for (final PackageTreeNode node : actual) {
			expectedNsURIs.remove(node.getNSURI());
		}
		if (expectedNsURIs.isEmpty()) {
			return;
		}
		final StringBuilder stringBuilder = new StringBuilder(
			"The following expected nsURIs were not in the actual set: ");
		for (final String uri : expectedNsURIs) {
			stringBuilder.append(uri + " ");
		}
		fail(stringBuilder.toString());
	}

}
