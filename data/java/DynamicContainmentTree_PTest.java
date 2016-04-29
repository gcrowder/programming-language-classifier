/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 */
package org.eclipse.emf.ecp.view.dynamictree.model.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainIntermediate;
import org.eclipse.emf.ecp.view.dynamictree.model.DomainRoot;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentItem;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTree;
import org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelFactory;
import org.eclipse.emf.ecp.view.dynamictree.model.ModelPackage;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElement;
import org.eclipse.emf.ecp.view.dynamictree.model.TestElementContainer;
import org.eclipse.emf.ecp.view.spi.categorization.model.VAction;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class DynamicContainmentTree_PTest {

	private DomainRoot root;
	// private Node<DynamicContainmentTree> node;
	private DynamicContainmentTree tree;

	private static final String ELEMENT_CONTAINER_ID = "111";
	private static final String ELEMENT_ID = "222";

	private DefaultRealm realm;

	@Before
	public void setUp() throws Exception {
		realm = new DefaultRealm();
		root = ModelFactory.eINSTANCE.createDomainRoot();
		final DomainIntermediate intermediate = ModelFactory.eINSTANCE.createDomainIntermediate();
		root.setIntermediate(intermediate);

		final TestElementContainer elementContainer = ModelFactory.eINSTANCE.createTestElementContainer();
		elementContainer.setId(ELEMENT_CONTAINER_ID);
		intermediate.setTestElementContainer(elementContainer);

		final TestElement testElement = ModelFactory.eINSTANCE.createTestElement();
		testElement.setId(ELEMENT_ID);
		testElement.setParentId(ELEMENT_CONTAINER_ID);
		elementContainer.getTestElements().add(testElement);

		tree = ModelFactory.eINSTANCE.createDynamicContainmentTree();
		tree.getPathToRoot().add(ModelPackage.eINSTANCE.getDomainRoot_Intermediate());
		tree.getPathToRoot().add(ModelPackage.eINSTANCE.getDomainIntermediate_TestElementContainer());
		tree.setChildReference(ModelPackage.eINSTANCE.getTestElementContainer_TestElements());

		// used for validation
		final VControl childNameControl = VViewFactory.eINSTANCE.createControl();
		childNameControl.setDomainModelReference(
			createDynamicContainmentTreeDomainModelReferences(
				ModelPackage.eINSTANCE.getTestElement_Name(),
				ModelPackage.eINSTANCE.getTestElementContainer_TestElements(),
				ModelPackage.eINSTANCE.getDomainRoot_Intermediate(),
				ModelPackage.eINSTANCE.getDomainIntermediate_TestElementContainer()));
		tree.setChildComposite(childNameControl);

		// set up scoping
		final VControl viewControl = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference createFeaturePathDomainModelReference = createFeaturePathDomainModelReference(
			ModelPackage.eINSTANCE.getTestElementContainer_Id(),
			ModelPackage.eINSTANCE.getDomainRoot_Intermediate(),
			ModelPackage.eINSTANCE.getDomainIntermediate_TestElementContainer());
		viewControl.setDomainModelReference(createFeaturePathDomainModelReference);
		tree.setComposite(viewControl);

		final ViewModelContext viewModelContext = ViewModelContextFactory.INSTANCE.createViewModelContext(tree, root);
	}

	@After
	public void tearDown() throws Exception {
		realm.dispose();
	}

	private VFeaturePathDomainModelReference createFeaturePathDomainModelReference(
		EStructuralFeature feature,
		EReference... references) {
		final VFeaturePathDomainModelReference result = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		result.setDomainModelEFeature(feature);
		result.getDomainModelEReferencePath().addAll(Arrays.asList(references));
		return result;
	}

	private DynamicContainmentTreeDomainModelReference createDynamicContainmentTreeDomainModelReferences(
		EStructuralFeature base, EStructuralFeature rootFeature, EReference... toRoot) {
		final DynamicContainmentTreeDomainModelReference reference = ModelFactory.eINSTANCE
			.createDynamicContainmentTreeDomainModelReference();
		final VFeaturePathDomainModelReference rootRef = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		final VFeaturePathDomainModelReference baseRef = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		rootRef.getDomainModelEReferencePath().addAll(Arrays.asList(toRoot));
		rootRef.setDomainModelEFeature(rootFeature);
		baseRef.setDomainModelEFeature(base);
		reference.setPathFromRoot(rootRef);
		reference.setPathFromBase(baseRef);
		return reference;
	}

	// @Test
	// public void testFilter() {
	//
	// final List<Node<?>> filterVisibleNodes = CategorizationFilterHelper
	// .filterNodes(node);
	//
	// assertEquals(1, filterVisibleNodes.size());
	// }

	@Test
	public void testAddNodeToDomainNode() {
		final String id = "123";
		addItem(id, tree, tree.getDomainModel());
		// final List<Node<?>> children = node.getChildren();
		// final Node<?> lastChild = children.get(children.size() - 1);
		final Object labelObjectOfLastChild = tree.getItems().get(tree.getItems().size() - 1).getDomainModel();
		assertTrue(TestElement.class.isInstance(labelObjectOfLastChild));

		final TestElement testElement = (TestElement) labelObjectOfLastChild;

		assertEquals(id, testElement.getId());
		assertEquals(ELEMENT_CONTAINER_ID, testElement.getParentId());
	}

	@Test
	public void testValidationErrorInit() {
		final String id = "123";
		addItem(id, tree, tree.getDomainModel());
		assertEquals("Severity must be error", Diagnostic.ERROR,
			tree.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationErrorToError() {
		final String id = "123";
		addItem(id, tree, tree.getDomainModel());
		// final List<Node<?>> children = node.getChildren();
		// final Node<?> lastChild = children.get(children.size() - 1);
		final Object labelObjectOfLastChild = tree.getItems().get(tree.getItems().size() - 1).getDomainModel();
		assertTrue(TestElement.class.isInstance(labelObjectOfLastChild));

		final TestElement testElement = (TestElement) labelObjectOfLastChild;
		testElement.setName("foo");

		assertEquals("Severity must be error", Diagnostic.ERROR,
			tree.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationErrorToOkByRemove() {
		assertEquals("Severity must be error", Diagnostic.ERROR,
			tree.getDiagnostic().getHighestSeverity());
		// final String id = "123";
		// addItem(id, tree, tree.getDomainModel());
		final Object labelObjectOfLastChild = tree.getItems().get(tree.getItems().size() - 1).getDomainModel();
		assertTrue(TestElement.class.isInstance(labelObjectOfLastChild));

		final TestElement testElement = (TestElement) labelObjectOfLastChild;
		final TestElementContainer container = (TestElementContainer) testElement.eContainer();

		container.getTestElements().clear();
		// TODO remove must be somewhere else
		tree.getItems().remove(tree.getItems().size() - 1);

		assertEquals("Severity must be ok", Diagnostic.OK,
			tree.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidationErrorToOk() {
		final String id = "123";
		addItem(id, tree, tree.getDomainModel());
		final Object labelObjectOfLastChild = tree.getItems().get(tree.getItems().size() - 1).getDomainModel();
		assertTrue(TestElement.class.isInstance(labelObjectOfLastChild));

		final TestElement testElement = (TestElement) labelObjectOfLastChild;
		for (final TestElement el : ((TestElementContainer) testElement.eContainer()).getTestElements()) {
			el.setName("bar");
		}

		assertEquals("Severity must be ok", Diagnostic.OK,
			tree.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testAddNodeToTestElement() {
		final String id = "123456789012";
		final DynamicContainmentItem dynamicContainmentItem = tree.getItems().get(tree.getItems().size() - 1);
		addItem(id, dynamicContainmentItem, dynamicContainmentItem.getDomainModel());

		final Object labelObject = dynamicContainmentItem.getItems().get(dynamicContainmentItem.getItems().size() - 1)
			.getDomainModel();
		assertTrue(TestElement.class.isInstance(labelObject));

		final TestElement testElement = (TestElement) labelObject;

		assertEquals(id, testElement.getId());
		assertEquals(ELEMENT_ID, testElement.getParentId());
	}

	@Test
	public void testRemoveNodeFromTestElement() {
		final DynamicContainmentItem dynamicContainmentItem = tree.getItems().get(tree.getItems().size() - 1);
		removeItem(dynamicContainmentItem, dynamicContainmentItem.getDomainModel());

		assertEquals(0, tree.getItems().size());
	}

	private void removeItem(VElement treeItem, EObject treeItemDomain) {
		final EObject eObject = treeItemDomain.eContainer();
		if (TestElementContainer.class.isInstance(eObject)) {
			TestElementContainer.class.cast(eObject).getTestElements().remove(treeItemDomain);
		} else if (TestElement.class.isInstance(eObject)) {
			TestElement.class.cast(eObject).getElements().remove(treeItemDomain);
		}

		final EObject treeItemParent = treeItem.eContainer();
		if (DynamicContainmentTree.class.isInstance(treeItemParent)) {
			DynamicContainmentTree.class.cast(treeItemParent).getItems().remove(treeItem);
		} else if (DynamicContainmentItem.class.isInstance(treeItemParent)) {
			DynamicContainmentItem.class.cast(treeItemParent).getItems().remove(treeItem);
		}

	}

	public static DynamicContainmentItem addItem(String id, VElement viewModelParent, EObject virtualParent) {

		final TestElement newValue = ModelFactory.eINSTANCE.createTestElement();
		newValue.setParentId((String) virtualParent.eGet(virtualParent
			.eClass().getEStructuralFeature("id")));
		newValue.setId(id);

		VElement renderable = null;
		DynamicContainmentTree tree = null;
		List<VAction> actions = null;

		if (!TestElementContainer.class.isInstance(virtualParent)) {
			virtualParent = virtualParent.eContainer();
			final DynamicContainmentItem item = (DynamicContainmentItem) viewModelParent;

			EObject parent = item.eContainer();
			while (!DynamicContainmentTree.class.isInstance(parent)) {
				parent = parent.eContainer();
			}
			tree = (DynamicContainmentTree) parent;
		}

		if (tree == null) {
			tree = (DynamicContainmentTree) viewModelParent;
		}
		actions = tree.getActions();
		renderable = tree.getChildComposite();

		final DynamicContainmentItem pi = ModelFactory.eINSTANCE.createDynamicContainmentItem();
		pi.setComposite((VContainedElement) EcoreUtil.copy(renderable));
		pi.setDomainModel(newValue);
		if (DynamicContainmentItem.class.isInstance(viewModelParent)) {
			final DynamicContainmentItem parent = (DynamicContainmentItem) viewModelParent;
			parent.getItems().add(pi);
		} else {
			tree.getItems().add(pi);
		}

		((TestElementContainer) virtualParent).getTestElements().add(newValue);
		return pi;
	}

}
