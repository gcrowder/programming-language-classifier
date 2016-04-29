/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.categorization.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTView;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.categorization.model.VAbstractCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationFactory;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.model.ModelReferenceHelper;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(DatabindingClassRunner.class)
public class Categorization_PTest {

	private static final String TESTNAME = "test"; //$NON-NLS-1$
	private static final String NAME = "Name"; //$NON-NLS-1$
	private static final String PLAYERNAME = "Player"; //$NON-NLS-1$
	private Player player;
	private VView view;
	private VCategorizationElement element;

	@Before
	public void setUp() throws Exception {

		player = BowlingFactory.eINSTANCE.createPlayer();
		player.setName(PLAYERNAME);
		player.setGender(Gender.MALE);

		view = VViewFactory.eINSTANCE.createView();
		element = VCategorizationFactory.eINSTANCE.createCategorizationElement();
		view.getChildren().add(element);
	}

	private Control render() throws ECPRendererException {
		final Shell shell = new Shell();

		final ECPSWTView render = ECPSWTViewRenderer.INSTANCE.render(shell, player, view);
		return render.getSWTControl();
	}

	private static boolean checkIfThereIsATextControl(Control control) {
		if (Text.class.isInstance(control)) {
			return true;
		}

		Composite controlComposite = (Composite) control;
		if (Text.class.isInstance(Composite.class.cast(controlComposite.getChildren()[2]).getChildren()[0])) {
			return true;
		}
		controlComposite = (Composite) controlComposite.getChildren()[2];
		controlComposite = (Composite) controlComposite.getChildren()[0];
		controlComposite = (Composite) controlComposite.getChildren()[0];
		final Control textControl = controlComposite.getChildren()[0];

		return textControl instanceof Text;
	}

	private static void assertTreeAndTreeItems(Composite composite, VAbstractCategorization... categorizations) {
		assertTrue(composite.getChildren().length == 2);
		assertTrue(Tree.class.isInstance(composite.getChildren()[0]));
		final Tree tree = (Tree) composite.getChildren()[0];
		assertEquals(categorizations.length, tree.getItems().length);
		for (int i = 0; i < categorizations.length; i++) {
			checkTreeItem(tree.getItem(i), categorizations[i]);
		}
	}

	private static void checkTreeItem(TreeItem treeItem, VAbstractCategorization abstractCategorization) {
		assertEquals(abstractCategorization, treeItem.getData());
		if (VCategorization.class.isInstance(abstractCategorization)) {
			final VCategorization categorization = (VCategorization) abstractCategorization;
			assertEquals(categorization.getCategorizations().size(), treeItem.getItems().length);
			for (int i = 0; i < categorization.getCategorizations().size(); i++) {
				checkTreeItem(treeItem.getItem(i), categorization.getCategorizations().get(i));
			}
		}
	}

	private void assertLabelAndTextControl(Composite composite, String labelText) {
		final Composite composite2 = Composite.class.cast(composite.getChildren()[0]);
		assertLabelText(composite2, labelText);
		assertTrue(checkIfThereIsATextControl(composite2));
	}

	private void assertLabelText(Composite composite, String labelText) {
		assertTrue(Label.class.isInstance(composite.getChildren()[0]));
		assertEquals(labelText, ((Label) composite.getChildren()[0]).getText());
	}

	private void assertCategorizationSubEditor(Composite composite, String text) {
		assertTrue(ScrolledComposite.class.isInstance(composite.getChildren()[1]));
		final ScrolledComposite scrolledComposite = (ScrolledComposite) composite.getChildren()[1];
		Control content = scrolledComposite.getContent();
		assertTrue(Composite.class.isInstance(content));
		content = ((Composite) content).getChildren()[0];
		assertTrue(Composite.class.isInstance(content));
		assertEquals(2, ((Composite) content).getChildren().length);
		assertLabelText((Composite) content, text);
		assertTrue(Label.class.isInstance(((Composite) content).getChildren()[1]));
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testOnlyOneCategory() throws ECPRendererException {
		final VCategory vCategory1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl controlPlayerName = VViewFactory.eINSTANCE.createControl();
		controlPlayerName.setDomainModelReference(ModelReferenceHelper
			.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
		vCategory1.setComposite(controlPlayerName);
		element.getCategorizations().add(vCategory1);

		final Control render = render();

		assertTrue(Composite.class.isInstance(render));
		final Composite composite = (Composite) render;
		// extra assert due to extra composite
		assertTrue(composite.getChildren().length == 1);
		// composite = (Composite) composite.getChildren()[0];

		assertTrue(Composite.class.cast(composite.getChildren()[0]).getChildren().length == 3);
		assertLabelAndTextControl(composite, NAME);

	}

	@Test
	public void testOnlyCategories() throws ECPRendererException {
		final VCategory vCategory1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl controlPlayerName = VViewFactory.eINSTANCE.createControl();
		controlPlayerName.setDomainModelReference(ModelReferenceHelper
			.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
		vCategory1.setComposite(controlPlayerName);
		element.getCategorizations().add(vCategory1);

		final VCategory vCategory2 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl controlPlayerHeight = VViewFactory.eINSTANCE.createControl();
		controlPlayerHeight.setDomainModelReference(ModelReferenceHelper
			.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Height()));
		vCategory2.setComposite(controlPlayerHeight);
		element.getCategorizations().add(vCategory2);

		final Control render = render();
		assertTrue(Composite.class.isInstance(render));
		Composite composite = (Composite) render;
		// extra assert due to extra composite
		assertTrue(composite.getChildren().length == 1);
		composite = (Composite) composite.getChildren()[0];

		assertTreeAndTreeItems(composite, vCategory1, vCategory2);

		assertTrue(ScrolledComposite.class.isInstance(composite.getChildren()[1]));
		final ScrolledComposite scrolledComposite = (ScrolledComposite) composite.getChildren()[1];
		final Control content = scrolledComposite.getContent();
		assertTrue(Composite.class.isInstance(content));
		// content = ((Composite) content).getChildren()[0];
		assertLabelAndTextControl((Composite) content, NAME);
	}

	@Test
	public void testCategorizationWithoutCategory() throws ECPRendererException {
		final VCategorization vCategorization = VCategorizationFactory.eINSTANCE.createCategorization();
		vCategorization.setName(TESTNAME);
		element.getCategorizations().add(vCategorization);
		final Control render = render();

		assertTrue(Composite.class.isInstance(render));
		Composite composite = (Composite) render;

		// extra assert due to extra composite
		assertTrue(composite.getChildren().length == 1);
		composite = (Composite) composite.getChildren()[0];

		assertTreeAndTreeItems(composite, vCategorization);

		assertCategorizationSubEditor(composite, TESTNAME);

	}

	@Test
	public void testCategorizationWithCategory() throws ECPRendererException {
		final VCategorization vCategorization = VCategorizationFactory.eINSTANCE.createCategorization();
		vCategorization.setName(TESTNAME);
		element.getCategorizations().add(vCategorization);
		final VCategory vCategory1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl controlPlayerName = VViewFactory.eINSTANCE.createControl();
		controlPlayerName.setDomainModelReference(ModelReferenceHelper
			.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
		vCategory1.setComposite(controlPlayerName);

		vCategorization.getCategorizations().add(vCategory1);

		final Control render = render();

		// asserts
		assertTrue(Composite.class.isInstance(render));
		Composite composite = (Composite) render;
		// extra assert due to extra composite
		assertTrue(composite.getChildren().length == 1);
		composite = (Composite) composite.getChildren()[0];

		assertTreeAndTreeItems(composite, vCategorization);

		assertCategorizationSubEditor(composite, TESTNAME);

	}

	@Test
	public void testCategorizationWithCategories() throws ECPRendererException {
		final VCategorization vCategorization = VCategorizationFactory.eINSTANCE.createCategorization();
		vCategorization.setName(TESTNAME);
		element.getCategorizations().add(vCategorization);
		final VCategory vCategory1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl controlPlayerName = VViewFactory.eINSTANCE.createControl();
		controlPlayerName.setDomainModelReference(ModelReferenceHelper
			.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
		vCategory1.setComposite(controlPlayerName);

		vCategorization.getCategorizations().add(vCategory1);

		final VCategory vCategory2 = VCategorizationFactory.eINSTANCE.createCategory();
		final VControl controlPlayerHeight = VViewFactory.eINSTANCE.createControl();
		controlPlayerHeight.setDomainModelReference(ModelReferenceHelper
			.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Height()));
		vCategory2.setComposite(controlPlayerHeight);
		vCategorization.getCategorizations().add(vCategory2);

		final Control render = render();

		// asserts
		assertTrue(Composite.class.isInstance(render));
		Composite composite = (Composite) render;
		// extra assert due to extra composite
		assertTrue(composite.getChildren().length == 1);
		composite = (Composite) composite.getChildren()[0];

		assertTreeAndTreeItems(composite, vCategorization);

		assertCategorizationSubEditor(composite, TESTNAME);

	}

	@Test
	public void testCategorizationsAndCategories() throws ECPRendererException {
		final VCategorization[] categorizations = new VCategorization[5];
		for (int i = 0; i < 5; i++) {
			final VCategorization vCategorization = VCategorizationFactory.eINSTANCE.createCategorization();
			vCategorization.setName(TESTNAME + i);
			element.getCategorizations().add(vCategorization);
			categorizations[i] = vCategorization;
			for (int j = 0; j < 5; j++) {
				final VCategorization vCategorization2 = VCategorizationFactory.eINSTANCE.createCategorization();
				vCategorization2.setName(TESTNAME + i + "_" + j); //$NON-NLS-1$
				vCategorization.getCategorizations().add(vCategorization2);

				final VCategory vCategory1 = VCategorizationFactory.eINSTANCE.createCategory();
				final VControl controlPlayerName = VViewFactory.eINSTANCE.createControl();
				controlPlayerName.setDomainModelReference(ModelReferenceHelper
					.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Name()));
				vCategory1.setComposite(controlPlayerName);

				vCategorization2.getCategorizations().add(vCategory1);

				final VCategory vCategory2 = VCategorizationFactory.eINSTANCE.createCategory();
				final VControl controlPlayerHeight = VViewFactory.eINSTANCE.createControl();
				controlPlayerHeight.setDomainModelReference(ModelReferenceHelper
					.createDomainModelReference(BowlingPackage.eINSTANCE.getPlayer_Height()));
				vCategory2.setComposite(controlPlayerHeight);
				vCategorization2.getCategorizations().add(vCategory2);

			}
		}

		final Control render = render();

		// asserts
		assertTrue(Composite.class.isInstance(render));
		Composite composite = (Composite) render;
		// extra assert due to extra composite
		assertTrue(composite.getChildren().length == 1);
		composite = (Composite) composite.getChildren()[0];

		assertTreeAndTreeItems(composite, categorizations);

		assertCategorizationSubEditor(composite, categorizations[0].getName());
	}

}
