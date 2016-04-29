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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Arrays;

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
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Eugen Neufeld
 *
 */
@RunWith(DatabindingClassRunner.class)
public class CategorizationTreeRefresh_PTest {

	private static Image idCategorization;
	private static Image idCategory;

	static {
		try {
			idCategorization = ImageDescriptor
				.createFromURL(
					new URL(
						"platform:/plugin/org.eclipse.emf.ecp.view.categorization.model.edit/icons/full/obj16/Categorization.gif"))
				.createImage();
			idCategory = ImageDescriptor
				.createFromURL(
					new URL(
						"platform:/plugin/org.eclipse.emf.ecp.view.categorization.model.edit/icons/full/obj16/Category.gif"))
				.createImage();
		} catch (final MalformedURLException ex) {
			ex.printStackTrace();
		}
	}

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
			final VFeaturePathDomainModelReference featurePathDomainModelReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			featurePathDomainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_EMails());
			control1.setDomainModelReference(featurePathDomainModelReference);
			category1.setComposite(control1);
		}
		{
			final VControl control1 = VViewFactory.eINSTANCE.createControl();
			final VFeaturePathDomainModelReference featurePathDomainModelReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			featurePathDomainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_EMails());
			control1.setDomainModelReference(featurePathDomainModelReference);
			category2.setComposite(control1);
		}

		player = BowlingFactory.eINSTANCE.createPlayer();

		shell = new Shell();
		shell.setLayout(new FillLayout());

	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		ecpSwtView.dispose();
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

	private boolean checkData(TreeItem item) {
		if (item.getData() == categorization) {
			return Arrays.equals(idCategorization.getImageData().data, item.getImage().getImageData().data);
		} else if (item.getData() == category1 || item.getData() == category2) {
			return Arrays.equals(idCategory.getImageData().data, item.getImage().getImageData().data);
		}
		return false;
	}

	private void assertNoError(Tree tree) {
		for (final TreeItem item : tree.getItems()) {
			assertTrue("there should not be an error icon!", checkData(item));
		}
	}

	private void assertError(Tree tree) {
		for (final TreeItem item : tree.getItems()) {
			assertFalse("the icon must have an error icon!", checkData(item));
		}
	}

	@Test
	public void testNoValidationErrorOnInit() throws ECPRendererException {
		player.getEMails().add("bla");
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		waitForUIThread();
		assertNoError(tree);
	}

	@Test
	public void testValidationErrorOnInit() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert
		waitForUIThread();
		assertError(tree);
	}

	@Test
	public void testValidationErrorDynamic() throws ECPRendererException {
		player.getEMails().add("bla");
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertNoError(tree);

		// change state
		player.getEMails().clear();

		// assert
		waitForUIThread();
		assertError(tree);
	}

	@Test
	public void testNoValidationErrorDynamic() throws ECPRendererException {
		final ViewModelContext vmc = ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		ecpSwtView = ECPSWTViewRenderer.INSTANCE.render(shell, vmc);
		final Tree tree = getTree(ecpSwtView.getSWTControl());

		// assert init state
		assertError(tree);

		// change state
		player.getEMails().add("bla");

		// assert
		waitForUIThread();
		assertNoError(tree);
	}

	private static void waitForUIThread() {
		final long maxTime = System.currentTimeMillis() + 5000;
		while (Display.getDefault().readAndDispatch()) {
			if (System.currentTimeMillis() > maxTime) {
				fail("Timeout");
			}
		}
	}
}
