/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.unset.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.internal.rule.RuleService;
import org.eclipse.emf.ecp.view.internal.rule.RuleServiceHelperImpl;
import org.eclipse.emf.ecp.view.internal.unset.UnsetService;
import org.eclipse.emf.ecp.view.rule.test.CommonRuleTest;
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
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author jfaltermeier
 *
 */
public class UnsetRuleIntegration_PTest extends CommonRuleTest {

	private static final String FOO = "foo";
	private Fan fan;
	private Merchandise merchandise;

	private final EStructuralFeature merchandiseNameFeature = BowlingPackage.eINSTANCE.getMerchandise_Name();
	private final EStructuralFeature fanNameFeature = BowlingPackage.eINSTANCE.getFan_Name();
	private final BigDecimal price = new BigDecimal(19.84);
	private final String mercName = "Wimpel";
	private final String fanName = "Max Morlock";

	private VView view;

	private ViewModelContext context;
	private DefaultRealm realm;

	@Before
	public void before() {
		realm = new DefaultRealm();
		fan = BowlingFactory.eINSTANCE.createFan();
		merchandise = BowlingFactory.eINSTANCE.createMerchandise();
		merchandise.setPrice(price);
		merchandise.setName(mercName);
		fan.setFavouriteMerchandise(merchandise);
		fan.setName(fanName);

		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(fan.eClass());
	}

	@After
	public void after() {
		if (context != null) {
			context.dispose();
		}
		realm.dispose();
	}

	@Test
	public void testPriorities() {
		final RuleService rule = new RuleService();
		final UnsetService unset = new UnsetService();
		assertTrue(rule.getPriority() < unset.getPriority());
	}

	@Test
	public void testUnset() {
		final VControl control1 = addControlToView(merchandiseNameReferenceFromFan());
		addShowRule(control1, true, BowlingPackage.eINSTANCE.getFan_Name(), FOO);

		fan.setName(FOO);
		merchandise.setName("bar");

		services(fan);

		fan.setName("quux");
		assertFalse(control1.isVisible());
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
	}

	@Test
	public void testMultiUnset() {
		final League league = BowlingFactory.eINSTANCE.createLeague();
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		league.getPlayers().add(player);

		view.setRootEClass(BowlingPackage.eINSTANCE.getLeague());

		final VControl control = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Players());
		control.setDomainModelReference(domainModelReference);

		view.getChildren().add(control);

		addShowRule(control, true, BowlingPackage.eINSTANCE.getLeague_Name(), "League");
		league.setName("League");
		services(league);
		assertEquals(1, league.getPlayers().size());
		league.setName("Liga");
		assertEquals(0, league.getPlayers().size());
	}

	@Test
	public void testInitUnset() {
		final VControl control1 = addControlToView(merchandiseNameReferenceFromFan());
		addShowRule(control1, true, BowlingPackage.eINSTANCE.getFan_Name(), FOO);

		merchandise.setName("bar");
		fan.setName("quux");
		services(fan);
		assertFalse(control1.isVisible());
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
	}

	@Test
	public void testVCategorizationElementHideCategory() {
		// set up view model
		final VCategorizationElement element = VCategorizationFactory.eINSTANCE.createCategorizationElement();
		view.getChildren().add(element);

		final VCategorization categorization = VCategorizationFactory.eINSTANCE.createCategorization();
		element.getCategorizations().add(categorization);

		final VCategory categoryInCategorization = VCategorizationFactory.eINSTANCE.createCategory();
		categorization.getCategorizations().add(categoryInCategorization);

		final VControl fanNameC = VViewFactory.eINSTANCE.createControl();
		categoryInCategorization.setComposite(fanNameC);
		final VFeaturePathDomainModelReference fanDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		fanDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Name());
		fanNameC.setDomainModelReference(fanDMR);

		final VCategory category = VCategorizationFactory.eINSTANCE.createCategory();
		element.getCategorizations().add(category);

		final VControl merchName = VViewFactory.eINSTANCE.createControl();
		category.setComposite(merchName);
		final VFeaturePathDomainModelReference merchDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		merchDMR.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		merchDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		merchName.setDomainModelReference(merchDMR);

		// set up rules
		addShowRule(category, false, BowlingPackage.eINSTANCE.getFan_Name(), FOO);

		// init services
		services(fan);
		assertTrue(element.isVisible());
		assertTrue(categorization.isVisible());
		assertTrue(categoryInCategorization.isVisible());
		assertTrue(fanNameC.isVisible());
		assertTrue(category.isVisible());
		assertTrue(merchName.isVisible());
		assertTrue(fan.eIsSet(fanNameFeature));
		assertTrue(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanName, fan.getName());
		assertEquals(mercName, merchandise.getName());

		// act
		fan.setName(FOO);

		// assert
		assertTrue(element.isVisible());
		assertTrue(categorization.isVisible());
		assertTrue(categoryInCategorization.isVisible());
		assertTrue(fanNameC.isVisible());
		assertFalse(category.isVisible());
		assertFalse(merchName.isVisible());
		assertTrue(fan.eIsSet(fanNameFeature));
		assertEquals(FOO, fan.getName());
		assertFalse(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(merchandiseNameFeature.getDefaultValue(), merchandise.getName());
	}

	@Test
	public void testVCategorizationElementHideCategoryInCategorization() {
		// set up view model
		final VCategorizationElement element = VCategorizationFactory.eINSTANCE.createCategorizationElement();
		view.getChildren().add(element);

		final VCategorization categorization = VCategorizationFactory.eINSTANCE.createCategorization();
		element.getCategorizations().add(categorization);

		final VCategory categoryInCategorization = VCategorizationFactory.eINSTANCE.createCategory();
		categorization.getCategorizations().add(categoryInCategorization);

		final VControl fanNameC = VViewFactory.eINSTANCE.createControl();
		categoryInCategorization.setComposite(fanNameC);
		final VFeaturePathDomainModelReference fanDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		fanDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Name());
		fanNameC.setDomainModelReference(fanDMR);

		final VCategory category = VCategorizationFactory.eINSTANCE.createCategory();
		element.getCategorizations().add(category);

		final VControl merchName = VViewFactory.eINSTANCE.createControl();
		category.setComposite(merchName);
		final VFeaturePathDomainModelReference merchDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		merchDMR.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		merchDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		merchName.setDomainModelReference(merchDMR);

		// set up rules
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(true);
		rule.setCondition(createLeafCondition(BowlingPackage.eINSTANCE.getMerchandise_Name(), FOO,
			BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise()));
		categoryInCategorization.getAttachments().add(rule);

		// init services
		services(fan);
		assertTrue(element.isVisible());
		assertTrue(categorization.isVisible());
		assertTrue(categoryInCategorization.isVisible());
		assertTrue(fanNameC.isVisible());
		assertTrue(category.isVisible());
		assertTrue(merchName.isVisible());
		assertTrue(fan.eIsSet(fanNameFeature));
		assertTrue(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(fanName, fan.getName());
		assertEquals(mercName, merchandise.getName());

		// act
		merchandise.setName(FOO);

		// assert
		assertTrue(element.isVisible());
		assertTrue(categorization.isVisible());
		assertFalse(categoryInCategorization.isVisible());
		assertFalse(fanNameC.isVisible());
		assertTrue(category.isVisible());
		assertTrue(merchName.isVisible());
		assertFalse(fan.eIsSet(fanNameFeature));
		assertEquals(fanNameFeature.getDefaultValue(), fan.getName());
		assertTrue(merchandise.eIsSet(merchandiseNameFeature));
		assertEquals(FOO, merchandise.getName());
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// Factory methods
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////

	private void services(EObject domain) {
		context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);

		final UnsetService unsetService = new UnsetService();
		unsetService.instantiate(context);

		final RuleService ruleService = new RuleService();
		final RuleServiceHelperImpl ruleServiceHelper = new RuleServiceHelperImpl();
		ruleService.instantiate(context);
		ruleServiceHelper.instantiate(context);
	}

	/**
	 * Adds a control with the given feature path domain model reference as a direct child of the view.
	 *
	 * @param domainModelReference
	 * @return the created control
	 */
	private VControl addControlToView(VFeaturePathDomainModelReference domainModelReference) {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(domainModelReference);
		view.getChildren().add(control);
		return control;
	}

	private VFeaturePathDomainModelReference merchandiseNameReferenceFromFan() {
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		domainModelReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		return domainModelReference;
	}

}
