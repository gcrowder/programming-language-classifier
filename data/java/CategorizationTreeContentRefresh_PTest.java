/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.categorization.swt.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTView;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationFactory;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Eugen Neufeld
 *
 */
@RunWith(DatabindingClassRunner.class)
public class CategorizationTreeContentRefresh_PTest {

	private static final String EXPECTED_NAME = "test"; //$NON-NLS-1$
	private static final Double EXPECTED_HEIGHT = 1.0;
	private static final Boolean EXPECTED_PROF = true;

	private static final String NOT_EXPECTED_NAME = "testX"; //$NON-NLS-1$
	private static final Double NOT_EXPECTED_HEIGHT = 2.0;
	private static final Boolean NOT_EXPECTED_PROF = false;

	private Player player;
	private VView view;
	private VCategorization categorization;
	private VCategory category1;
	private VCategory category2;
	private ECPSWTView ecpSwtView;
	private Shell shell;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		view = VViewFactory.eINSTANCE.createView();

		final VCategorizationElement categorizationElement = VCategorizationFactory.eINSTANCE
			.createCategorizationElement();
		categorization = VCategorizationFactory.eINSTANCE.createCategorization();

		category1 = VCategorizationFactory.eINSTANCE.createCategory();
		category2 = VCategorizationFactory.eINSTANCE.createCategory();

		categorization.getCategorizations().add(category1);

		categorizationElement.getCategorizations().add(categorization);
		categorizationElement.getCategorizations().add(category2);

		view.getChildren().add(categorizationElement);

		{
			final VControl control1 = VViewFactory.eINSTANCE.createControl();

			control1.setDomainModelReference(getDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
			category1.setComposite(control1);
		}
		{
			final VControl control1 = VViewFactory.eINSTANCE.createControl();
			control1.setDomainModelReference(getDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
			category2.setComposite(control1);
		}

		player = BowlingFactory.eINSTANCE.createPlayer();

		shell = new Shell(Display.getDefault());
		shell.setLayout(new GridLayout(1, false));
		// shell.open();

		categorization.getAttachments().add(createShowRule(BowlingPackage.eINSTANCE.getPlayer_Name(), EXPECTED_NAME));
		category1.getAttachments().add(createShowRule(BowlingPackage.eINSTANCE.getPlayer_Height(), EXPECTED_HEIGHT));
		category2.getAttachments().add(
			createShowRule(BowlingPackage.eINSTANCE.getPlayer_IsProfessional(), EXPECTED_PROF));

		player.setName(NOT_EXPECTED_NAME);
		player.setHeight(NOT_EXPECTED_HEIGHT);
		player.setIsProfessional(NOT_EXPECTED_PROF);
	}

	/**
	 * @param player_Name
	 * @return
	 */
	private ShowRule createShowRule(EStructuralFeature feature, Object expected) {
		final ShowRule showRuleCategorization = RuleFactory.eINSTANCE.createShowRule();
		final LeafCondition leafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		leafCondition.setDomainModelReference(getDomainModelReference(feature));
		showRuleCategorization.setCondition(leafCondition);
		leafCondition.setExpectedValue(expected);
		return showRuleCategorization;
	}

	private VFeaturePathDomainModelReference getDomainModelReference(EStructuralFeature feature) {
		final VFeaturePathDomainModelReference featurePathDomainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		featurePathDomainModelReference.setDomainModelEFeature(feature);
		return featurePathDomainModelReference;
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		ecpSwtView.dispose();
		shell.close();
		shell.dispose();
	}

	/**
	 * @param swtControl
	 * @return
	 */
	private Tree getTree(Control swtControl) {
		final Composite composite = (Composite) swtControl;
		return (Tree) ((Composite) composite.getChildren()[0]).getChildren()[0];
	}

	@Test
	public void testHideAllOnInit() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(0, tree.getItemCount());
	}

	@Test
	public void testShowAllOnInit() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);

		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
		assertEquals(category2, tree.getItems()[1].getData());
	}

	@Test
	public void testShowCategorizationWithoutChildOnInit() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(0, tree.getItems()[0].getItemCount());
	}

	@Test
	public void testShowCategorizationWithChildOnInit() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
	}

	@Test
	public void testShowCategoryAndCategorizationWithoutChildOnInit() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setIsProfessional(EXPECTED_PROF);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);

		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(0, tree.getItems()[0].getItemCount());
		assertEquals(category2, tree.getItems()[1].getData());
	}

	@Test
	public void testHideCategorizationWithChildOnInit() throws ECPRendererException {
		player.setHeight(EXPECTED_HEIGHT);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(0, tree.getItemCount());
	}

	@Test
	public void testShowRootCategoryHideCategorizationWithChildOnInit() throws ECPRendererException {
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(category2, tree.getItems()[0].getData());
	}

	@Test
	public void testShowRootCategoryHideCategorizationWithoutChildOnInit() throws ECPRendererException {
		player.setIsProfessional(EXPECTED_PROF);
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(category2, tree.getItems()[0].getData());
	}

	@Test
	public void testHideAllDynamic() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);

		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		ecpSwtView.getSWTControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
		assertEquals(category2, tree.getItems()[1].getData());

		// change state
		player.setName(NOT_EXPECTED_NAME);
		player.setHeight(NOT_EXPECTED_HEIGHT);
		player.setIsProfessional(NOT_EXPECTED_PROF);

		while (Display.getDefault().readAndDispatch()) {
		}
		// assert
		assertEquals(0, tree.getItemCount());
	}

	@Test
	public void testHideCategorizationDynamic() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);

		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		ecpSwtView.getSWTControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
		assertEquals(category2, tree.getItems()[1].getData());

		// change state
		player.setName(NOT_EXPECTED_NAME);

		while (Display.getDefault().readAndDispatch()) {
		}
		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(category2, tree.getItems()[0].getData());
	}

	@Test
	public void testHideSubCategoryDynamic() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);

		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		ecpSwtView.getSWTControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
		assertEquals(category2, tree.getItems()[1].getData());

		// change state
		player.setHeight(NOT_EXPECTED_HEIGHT);

		while (Display.getDefault().readAndDispatch()) {
		}
		// assert
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(0, tree.getItems()[0].getItemCount());
		assertEquals(category2, tree.getItems()[1].getData());
	}

	@Test
	public void testHideRootCategoryDynamic() throws ECPRendererException {
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);

		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		ecpSwtView.getSWTControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
		assertEquals(category2, tree.getItems()[1].getData());

		// change state
		player.setIsProfessional(NOT_EXPECTED_PROF);

		while (Display.getDefault().readAndDispatch()) {
		}
		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		tree.showItem(tree.getItems()[0].getItems()[0]);
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
	}

	@Test
	public void testShowAllDynamic() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);

		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(0, tree.getItemCount());

		// change state
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);
		player.setIsProfessional(EXPECTED_PROF);

		while (Display.getDefault().readAndDispatch()) {
		}

		// assert
		assertEquals(2, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		tree.showItem(tree.getItems()[0].getItems()[0]);
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
		assertEquals(category2, tree.getItems()[1].getData());
	}

	@Test
	public void testShowCategorizationDynamic() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);

		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(0, tree.getItemCount());

		// change state
		player.setName(EXPECTED_NAME);

		while (Display.getDefault().readAndDispatch()) {
		}

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(0, tree.getItems()[0].getItemCount());
	}

	@Test
	public void testShowSubCategoryDynamic() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);

		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(0, tree.getItemCount());

		// change state
		player.setName(EXPECTED_NAME);
		player.setHeight(EXPECTED_HEIGHT);

		while (Display.getDefault().readAndDispatch()) {
		}

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(categorization, tree.getItems()[0].getData());
		assertEquals(1, tree.getItems()[0].getItemCount());
		tree.showItem(tree.getItems()[0].getItems()[0]);
		assertEquals(category1, tree.getItems()[0].getItems()[0].getData());
	}

	@Test
	public void testShowRootCategoryDynamic() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);

		shell.open();
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertEquals(0, tree.getItemCount());

		// change state
		player.setIsProfessional(EXPECTED_PROF);

		while (Display.getDefault().readAndDispatch()) {
		}

		// assert
		assertEquals(1, tree.getItemCount());
		assertEquals(category2, tree.getItems()[0].getData());
	}
}
