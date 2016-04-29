/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.treemasterdetail.validation.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VDiagnostic;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.treemasterdetail.model.VTreeMasterDetail;
import org.eclipse.emf.ecp.view.treemasterdetail.model.VTreeMasterDetailFactory;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel1;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.ChildLevel2;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.Root;
import org.eclipse.emf.ecp.view.treemasterdetail.validation.test.TestTMD.TestTMDFactory;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Validation tests for TreeMasterDetail.
 *
 * @author Eugen Neufeld
 *
 */
public class TreeMasterDetailValidation_ITest {

	private static final String VALID_NAME = "test"; //$NON-NLS-1$

	private DefaultRealm defaultRealm;

	@Before
	public void setup() {
		defaultRealm = new DefaultRealm();
	}

	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	@Test
	public void testInvalidRoot() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
	}

	@Test
	public void testValidRoot() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
	}

	@Test
	public void testInvalidRootInvalidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(2, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(root, diagnostic.getDiagnostics(root).get(1).getData()
			.get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidRootInvalidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testInvalidRootValidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(root, diagnostic.getDiagnostics(root).get(0).getData()
			.get(0));
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidRootValidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidRootInvalidChildren() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);
		final ChildLevel1 childLevel12 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel12);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel12, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(1, diagnostic.getDiagnostics(childLevel12).size());
	}

	@Test
	public void testInvalidChildInvalidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel2);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(2, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(childLevel2, diagnostic.getDiagnostics(root).get(1)
			.getData().get(0));
		assertEquals(2, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(childLevel2, diagnostic.getDiagnostics(childLevel1).get(1)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidChildInvalidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel2);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidChildValidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidChildValidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidRootToValid() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		root.setName(VALID_NAME);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
	}

	@Test
	public void testValidRootToInvalid() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		root.setName(null);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
	}

	@Test
	public void testInvalidChildToValid() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		childLevel1.setName(VALID_NAME);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidChildToInvalid() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		childLevel1.setName(null);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidRootAddValidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidRootAddInvalidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testInvalidRootAddValidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testInvalidRootAddInvalidChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(2, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(root, diagnostic.getDiagnostics(root).get(1).getData()
			.get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
	}

	@Test
	public void testValidRootAddValidSubTree() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidRootAddInvalidSubTree() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidRootAddInvalidSubTree2() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidRootAddValidSubTree() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidRootAddInvalidSubTree1() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(2, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(root, diagnostic.getDiagnostics(root).get(1).getData()
			.get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidRootAddInvalidSubTree2() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(2, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(root, diagnostic.getDiagnostics(root).get(1).getData()
			.get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidRootWithChildAddInvalidSibling() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel12 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel12);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel12, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(1, diagnostic.getDiagnostics(childLevel12).size());
	}

	@Test
	public void testValidChildAddValidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.OK, diagnostic.getHighestSeverity(), 0);
		assertEquals(0, diagnostic.getDiagnostics(root).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidChildAddInvalidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel2);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel2, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidChildAddValidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testInvalidChildAddInvalidSubChild() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		root.getChildren().add(childLevel1);

		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel2);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(2, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(childLevel2, diagnostic.getDiagnostics(root).get(1)
			.getData().get(0));
		assertEquals(2, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(childLevel2, diagnostic.getDiagnostics(childLevel1).get(1)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel2).size());
	}

	@Test
	public void testValidChildWithSubChildAddInvalidSibling() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		final ChildLevel2 childLevel2 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel2.setName(VALID_NAME);
		childLevel1.getChildren().add(childLevel2);

		root.getChildren().add(childLevel1);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel2 childLevel22 = TestTMDFactory.eINSTANCE.createChildLevel2();
		childLevel1.getChildren().add(childLevel22);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel22, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
		assertEquals(childLevel22, diagnostic.getDiagnostics(childLevel1).get(0)
			.getData().get(0));
		assertEquals(0, diagnostic.getDiagnostics(childLevel2).size());
		assertEquals(1, diagnostic.getDiagnostics(childLevel22).size());
	}

	@Test
	public void testValidRootAddValidChildToInvalid() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final Root root = TestTMDFactory.eINSTANCE.createRoot();
		root.setName(VALID_NAME);
		view.setRootEClass(root.eClass());

		final VTreeMasterDetail control = VTreeMasterDetailFactory.eINSTANCE
			.createTreeMasterDetail();

		view.getChildren().add(control);

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, root);

		final ChildLevel1 childLevel1 = TestTMDFactory.eINSTANCE.createChildLevel1();
		childLevel1.setName(VALID_NAME);
		root.getChildren().add(childLevel1);

		childLevel1.setName(null);

		final VDiagnostic diagnostic = control.getDiagnostic();
		assertEquals(Diagnostic.ERROR, diagnostic.getHighestSeverity(), 0);
		assertEquals(1, diagnostic.getDiagnostics(root).size());
		assertEquals(childLevel1, diagnostic.getDiagnostics(root).get(0)
			.getData().get(0));
		assertEquals(1, diagnostic.getDiagnostics(childLevel1).size());
	}
}
