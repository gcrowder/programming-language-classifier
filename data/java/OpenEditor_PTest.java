/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.ui.editor.test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorization;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationElement;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategorizationFactory;
import org.eclipse.emf.ecp.view.spi.categorization.model.VCategory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.spi.GCCollectable;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.junit.AfterClass;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Test case that renders a view, disposes it and checks whether the view
 * is garbage collectable.
 *
 * @author emueller
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class OpenEditor_PTest extends ECPCommonSWTBotTest {

	private static double memBefore;
	private static double memAfter;

	private final boolean isDomainCollectable;

	private GCCollectable viewCollectable;
	private GCCollectable domainCollectable;

	public OpenEditor_PTest(boolean isDomainCollectable) {
		this.isDomainCollectable = isDomainCollectable;
	}

	@Parameters
	public static Collection<Object[]> data() {
		final List<Object[]> data = new ArrayList<Object[]>();
		for (int i = 0; i < 99; i++) {
			data.add(new Boolean[] { false });
		}
		data.add(new Boolean[] { true });
		return data;
	}

	@AfterClass
	public static void afterClass() {
		final double diff = Math.abs((memBefore - memAfter) / memBefore);
		assertTrue(diff < 0.05);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#logic()
	 */
	@Override
	public void logic() {
		final SWTBotTree tree = bot.tree();
		tree.getTreeItem("parent").getNode("foo").getNode("2").select();
	}

	@Override
	public void assertions(double before, double after) {
		OpenEditor_PTest.memBefore += before;
		OpenEditor_PTest.memAfter += after;

		if (getDomainObject() != null) {

			assertTrue("More than four adapter left on domain model element after dispose of ECPSWTView: "
				+ getDomainObject().eAdapters().size()
				+ " adapters. Not all adapters can be removed, but it's maybe time to get suspicious.",
				getDomainObject()
					.eAdapters().size() < 5);

		}

		assertTrue(getSWTViewCollectable().isCollectable());
		unsetSWTViewCollectable();
		unsetDomainObject();
		assertTrue(viewCollectable.isCollectable());
		viewCollectable = null;
		assertTrue(domainCollectable.isCollectable());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#createDomainObject()
	 */
	@Override
	public EObject createDomainObject() {
		Player player = (Player) getDomainObject();

		if (isDomainCollectable) {
			// remove reference to domain object, since gc will be tested
			setDomainObject(null);
		}

		if (player == null) {
			player = BowlingFactory.eINSTANCE.createPlayer();
			player.setName("Test");
			memBefore = 0d;
			memAfter = 0d;
		}

		if (!isDomainCollectable) {
			setDomainObject(player);
		}

		domainCollectable = new GCCollectable(player);
		return player;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#createView()
	 */
	@Override
	public VView createView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getPlayer());

		final VControl nameControl = createControl(BowlingPackage.eINSTANCE
			.getPlayer_Name());
		final VControl genderControl = createControl(BowlingPackage.eINSTANCE
			.getPlayer_Gender());
		final VControl heightControl = createControl(BowlingPackage.eINSTANCE
			.getPlayer_Height());
		final VControl victoriesControl = createControl(BowlingPackage.eINSTANCE
			.getPlayer_NumberOfVictories());

		view.getChildren().add(
			createCategorizations(nameControl, genderControl, heightControl,
				victoriesControl));

		viewCollectable = new GCCollectable(view);

		return view;
	}

	private VControl createControl(EStructuralFeature feature) {
		final org.eclipse.emf.ecp.view.spi.model.VControl control = VViewFactory.eINSTANCE
			.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(feature);
		control.setDomainModelReference(domainModelReference);
		return control;
	}

	private VCategorizationElement createCategorizations(
		org.eclipse.emf.ecp.view.spi.model.VContainedElement composite1,
		org.eclipse.emf.ecp.view.spi.model.VContainedElement composite2,
		org.eclipse.emf.ecp.view.spi.model.VContainedElement composite3,
		org.eclipse.emf.ecp.view.spi.model.VContainedElement composite4) {

		final VCategorizationElement categorizationElement = VCategorizationFactory.eINSTANCE
			.createCategorizationElement();

		final VCategorization parentCategorization = VCategorizationFactory.eINSTANCE
			.createCategorization();
		final VCategorization fooCategorization = VCategorizationFactory.eINSTANCE
			.createCategorization();
		final VCategorization barCategorization = VCategorizationFactory.eINSTANCE
			.createCategorization();
		final VCategory category1 = VCategorizationFactory.eINSTANCE.createCategory();
		final VCategory category2 = VCategorizationFactory.eINSTANCE.createCategory();
		final VCategory category3 = VCategorizationFactory.eINSTANCE.createCategory();
		final VCategory category4 = VCategorizationFactory.eINSTANCE.createCategory();
		parentCategorization.setName("parent");
		fooCategorization.setName("foo");
		barCategorization.setName("bar");
		category1.setName("1");
		category2.setName("2");
		category3.setName("3");
		category4.setName("4");
		category1.setComposite(composite1);
		category2.setComposite(composite2);
		category3.setComposite(composite3);
		category4.setComposite(composite4);
		fooCategorization.getCategorizations().add(category1);
		fooCategorization.getCategorizations().add(category2);
		barCategorization.getCategorizations().add(category3);
		barCategorization.getCategorizations().add(category4);
		parentCategorization.getCategorizations().add(fooCategorization);
		parentCategorization.getCategorizations().add(barCategorization);
		categorizationElement.getCategorizations().add(parentCategorization);
		return categorizationElement;
	}

}
