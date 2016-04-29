/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.view.vertical.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecp.ui.view.test.HierarchyViewModelHandle;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(DatabindingClassRunner.class)
public class SWTVertical_PTest {

	private Shell shell;
	private EObject domainElement;

	@Before
	public void init() {
		shell = SWTViewTestHelper.createShell();
		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		eClass.getESuperTypes().add(EcorePackage.eINSTANCE.getEClass());
		eClass.setInstanceClassName("Test");
		domainElement = eClass;
	}

	@Test
	public void testVerticalWithoutChildren() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final HierarchyViewModelHandle handle = createVerticalWithoutChildren();
		final Control render = SWTViewTestHelper.render(handle.getRoot(), domainElement, shell);
		assertTrue(render instanceof Composite);
		final Composite composite = (Composite) render;
		assertEquals(0, composite.getChildren().length);
	}

	@Test
	public void testVerticalWithTwoControlsAsChildren() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final HierarchyViewModelHandle handle = createVerticalWithTwoControlsAsChildren();
		final Control render = SWTViewTestHelper.render(handle.getRoot(), domainElement, shell);
		assertTrue(render instanceof Composite);
		final Composite composite = (Composite) render;
		assertEquals(6, composite.getChildren().length);
		assertEquals(3, SWTViewTestHelper.getNumberofColumns(composite));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(composite.getChildren()[2]));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(composite.getChildren()[5]));
	}

	@Test
	public void testVerticalWithTwoVerticalAsChildrenAndControlAsSubChildren() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final HierarchyViewModelHandle handle =
			createVerticalWithTwoVerticalAsChildrenAndControlAsSubChildren();
		final Control render = SWTViewTestHelper.render(handle.getRoot(), domainElement, shell);
		assertTrue(render instanceof Composite);
		final Composite composite = (Composite) render;
		assertEquals(2, composite.getChildren().length);
		final Composite firstVertical = (Composite) composite.getChildren()[0];
		final Composite secondVertical = (Composite) composite.getChildren()[1];

		assertEquals(1, SWTViewTestHelper.getHorizontalSpan(firstVertical));
		assertEquals(1, SWTViewTestHelper.getHorizontalSpan(secondVertical));

		assertEquals(6, firstVertical.getChildren().length);
		assertEquals(6, secondVertical.getChildren().length);
		assertEquals(3, SWTViewTestHelper.getNumberofColumns(firstVertical));
		assertEquals(3, SWTViewTestHelper.getNumberofColumns(secondVertical));

		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(firstVertical.getChildren()[2]));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(secondVertical.getChildren()[2]));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(firstVertical.getChildren()[5]));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(secondVertical.getChildren()[5]));
	}

	private static HierarchyViewModelHandle createVerticalWithTwoVerticalAsChildrenAndControlAsSubChildren() {
		final HierarchyViewModelHandle verticalHandle = createVerticalWithoutChildren();
		verticalHandle.addFirstChildToRoot(createVertical());
		verticalHandle.addSecondChildToRoot(createVertical());
		verticalHandle.addFirstChildToFirstChild(createControl());
		verticalHandle.addSecondChildToFirstChild(createControl());
		verticalHandle.addFirstChildToSecondChild(createControl());
		verticalHandle.addSecondChildToSecondChild(createControl());
		return verticalHandle;
	}

	private static HierarchyViewModelHandle createVerticalWithTwoControlsAsChildren() {
		final HierarchyViewModelHandle verticalHandle = createVerticalWithoutChildren();
		final VControl control1 = createControl();
		verticalHandle.addFirstChildToRoot(control1);
		final VControl control2 = createControl();
		verticalHandle.addSecondChildToRoot(control2);
		return verticalHandle;
	}

	private static VControl createControl() {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(EcorePackage.eINSTANCE.getEClassifier_InstanceClassName());
		control.setDomainModelReference(domainModelReference);
		return control;
	}

	private static HierarchyViewModelHandle createVerticalWithoutChildren() {
		final VElement vertical = createVertical();
		return new HierarchyViewModelHandle(vertical);
	}

	private static VVerticalLayout createVertical() {
		return VVerticalFactory.eINSTANCE.createVerticalLayout();
	}
}
