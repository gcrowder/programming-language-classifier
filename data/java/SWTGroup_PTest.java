/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.view.group.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecp.view.spi.group.model.VGroup;
import org.eclipse.emf.ecp.view.spi.group.model.VGroupFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(DatabindingClassRunner.class)
public class SWTGroup_PTest {

	private static final String GROUP_NAME = "group";
	private static final String GROUP_NAME2 = "group2";

	/**
	 * @return a View with one Group in it
	 */
	private static VView createViewWithOneGroup() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VGroup group = createGroup();
		view.getChildren().add(group);
		return view;
	}

	/**
	 * @return A view with two groups as children
	 */
	private static VView createViewWithTwoGroups() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VGroup group = createGroup();
		view.getChildren().add(group);
		final VGroup group2 = createGroup();
		view.getChildren().add(group2);
		return view;
	}

	/**
	 * @return A View with two groups, one is the subgroup of the first one
	 */
	private static VView createViewWithTwoHierachicalGroups() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VGroup group = createGroup();
		view.getChildren().add(group);
		final VGroup subGroup = createGroup();
		group.getChildren().add(subGroup);
		return view;
	}

	private static VView createViewWithTwoGroupsWithTwoControls() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VGroup group1 = createGroup();
		view.getChildren().add(group1);
		group1.getChildren().add(createControl());
		group1.getChildren().add(createControl());
		final VGroup group2 = createGroup();
		view.getChildren().add(group2);
		group2.getChildren().add(createControl());
		group2.getChildren().add(createControl());
		return view;
	}

	/**
	 * @return
	 */
	private static VGroup createGroup() {
		return VGroupFactory.eINSTANCE.createGroup();
	}

	private static VControl createControl() {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelReference.setDomainModelEFeature(EcorePackage.eINSTANCE.getEClassifier_InstanceClassName());
		control.setDomainModelReference(modelReference);
		return control;
	}

	@Test
	public void testOneGroupinView() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {

		// setup model
		final VView view = createViewWithOneGroup();
		final org.eclipse.emf.ecp.view.spi.group.model.VGroup group = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) view
			.getChildren()
			.get(0);
		group.setName(GROUP_NAME);

		// setup ui
		final Shell shell = SWTViewTestHelper.createShell();

		final Control control = SWTViewTestHelper.render(view, shell);
		final Composite viewComposite = (Composite) shell.getChildren()[0];
		final Control renderedControl = viewComposite.getChildren()[0];
		assertTrue("Rendered Control is not a Group", renderedControl instanceof org.eclipse.swt.widgets.Group);
		assertEquals(
			"Rendered Control and control returned by renderer are not the same",
			control, viewComposite);
		final Group groupControl = (Group) renderedControl;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME, groupControl.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl.getData("org.eclipse.rap.rwt.customVariant"));
	}

	@Test
	public void testTwoGroupsinView() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {

		// setup model
		final VView view = createViewWithTwoGroups();
		final org.eclipse.emf.ecp.view.spi.group.model.VGroup group = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) view
			.getChildren()
			.get(0);
		group.setName(GROUP_NAME);

		final org.eclipse.emf.ecp.view.spi.group.model.VGroup group2 = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) view
			.getChildren()
			.get(1);
		group2.setName(GROUP_NAME2);

		// setup ui
		final Shell shell = SWTViewTestHelper.createShell();

		final Control control = SWTViewTestHelper.render(view, shell);
		final Composite viewComposite = (Composite) shell.getChildren()[0];
		final Control renderedControl1 = viewComposite.getChildren()[0];
		final Control renderedControl2 = viewComposite.getChildren()[1];
		assertTrue("Rendered Control is not a Group", renderedControl1 instanceof org.eclipse.swt.widgets.Group);
		assertEquals(
			"Rendered Control and control returned by renderer are not the same",
			control, viewComposite);
		final Group groupControl1 = (Group) renderedControl1;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME, groupControl1.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl1.getData("org.eclipse.rap.rwt.customVariant"));

		assertTrue("Rendered Control is not a Group", renderedControl2 instanceof org.eclipse.swt.widgets.Group);

		final Group groupControl2 = (Group) renderedControl2;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME2, groupControl2.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl2.getData("org.eclipse.rap.rwt.customVariant"));
	}

	@Test
	public void testTwoGroupsHierachicalinView() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {

		final VView view = createViewWithTwoHierachicalGroups();
		final org.eclipse.emf.ecp.view.spi.group.model.VGroup group = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) view
			.getChildren()
			.get(0);
		final org.eclipse.emf.ecp.view.spi.group.model.VGroup subGroup = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) group
			.getChildren().get(0);
		group.setName(GROUP_NAME);

		subGroup.setName(GROUP_NAME2);

		// setup ui
		final Shell shell = SWTViewTestHelper.createShell();

		final Control control = SWTViewTestHelper.render(view, shell);
		final Composite viewComposite = (Composite) shell.getChildren()[0];
		final Composite renderedControl1 = (Composite) viewComposite.getChildren()[0];
		final Control renderedControl2 = renderedControl1.getChildren()[0];
		assertTrue("Rendered Control is not a Group", renderedControl1 instanceof org.eclipse.swt.widgets.Group);
		assertEquals(
			"Rendered Control and control returned by renderer are not the same",
			control, viewComposite);
		final Group groupControl1 = (Group) renderedControl1;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME, groupControl1.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl1.getData("org.eclipse.rap.rwt.customVariant"));

		assertTrue("Rendered Control is not a Group", renderedControl2 instanceof org.eclipse.swt.widgets.Group);

		final Group groupControl2 = (Group) renderedControl2;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME2, groupControl2.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl2.getData("org.eclipse.rap.rwt.customVariant"));
	}

	@Test
	public void testTwoGroupsWithTwoControlsInView() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		final VView view = createViewWithTwoGroupsWithTwoControls();
		final org.eclipse.emf.ecp.view.spi.group.model.VGroup group1 = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) view
			.getChildren()
			.get(0);
		final org.eclipse.emf.ecp.view.spi.group.model.VGroup group2 = (org.eclipse.emf.ecp.view.spi.group.model.VGroup) view
			.getChildren().get(1);
		group1.setName(GROUP_NAME);

		group2.setName(GROUP_NAME2);

		// setup ui
		final Shell shell = SWTViewTestHelper.createShell();

		final Control control = SWTViewTestHelper.render(view, EcoreFactory.eINSTANCE.createEClass(), shell);
		final Composite viewComposite = (Composite) shell.getChildren()[0];
		final Composite renderedControl1 = (Composite) viewComposite.getChildren()[0];
		final Composite renderedControl2 = (Composite) viewComposite.getChildren()[1];
		assertTrue("Rendered Control is not a Group", renderedControl1 instanceof org.eclipse.swt.widgets.Group);
		assertEquals(
			"Rendered Control and control returned by renderer are not the same",
			control, viewComposite);
		final Group groupControl1 = (Group) renderedControl1;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME, groupControl1.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl1.getData("org.eclipse.rap.rwt.customVariant"));

		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(renderedControl1.getChildren()[2]));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(renderedControl1.getChildren()[5]));

		assertTrue("Rendered Control is not a Group", renderedControl2 instanceof org.eclipse.swt.widgets.Group);

		final Group groupControl2 = (Group) renderedControl2;
		assertEquals("Rendered Group does not have correct name",
			GROUP_NAME2, groupControl2.getText());
		assertEquals("org_eclipse_emf_ecp_ui_control_group",
			renderedControl2.getData("org.eclipse.rap.rwt.customVariant"));

		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(renderedControl2.getChildren()[2]));
		assertTrue(SWTViewTestHelper.checkIfThereIsATextControl(renderedControl2.getChildren()[5]));
	}

	@Test
	public void testEmptyGroup() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {

		// setup model
		final VView view = createViewWithOneGroup();

		// setup ui
		final Shell shell = SWTViewTestHelper.createShell();
		try {
			final Composite control = (Composite) SWTViewTestHelper.render(view, shell);
			final org.eclipse.swt.widgets.Group renderedControl = (Group) control.getChildren()[0];
			assertEquals("", renderedControl.getText());
		} catch (final IllegalArgumentException e) {
			fail("Renderer throws IlleaglArgument on empty group name" + e.getStackTrace());
		}
	}
}
