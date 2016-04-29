/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.emf.ecp.common.spi.BidirectionalMap;
import org.eclipse.emf.ecp.common.spi.UniqueSetting;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.internal.rule.RuleRegistry;
import org.eclipse.emf.ecp.view.internal.rule.RuleService;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.Condition;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.OrCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emf.emfstore.bowling.Tournament;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests that assert that the {@link RuleRegistry}'s maps are updated correctly, especially on deletes.
 *
 * @author jfaltermeier
 *
 */
public class RuleRegistry_PTest {
	// assertions are pretty complex
	// BEGIN COMPLEX CODE
	private static final String CONDITION_TO_SETTINGS_FIELD_NAME = "conditionToSettings";
	private static final String RULES_TO_RENDERABLES_FIELD_NAME = "rulesToRenderables";
	private static final String SETTING_TO_RULES_FIELD_NAME = "settingToRules";
	private static final String REFLECTION_FAIL_FIX_TEST_SETUP = "Reflection fail. Fix test setup!";
	private static final String SHOW_RULE_REGISTRY_FIELD_NAME = "showRuleRegistry";
	private static final String FM_RIGHT = "Bowling Ball";
	private static final String M2_RIGHT = "Shirt";
	private static final String M1_RIGHT = "Scarf";

	private RuleService ruleService;

	private Fan fan;
	private Merchandise merchandise1;
	private Merchandise merchandise2;
	private Merchandise favMerchandise;

	private UniqueSetting favMerchandiseSetting;
	private UniqueSetting merchandise1Setting;
	private UniqueSetting merchandise2Setting;
	private UniqueSetting fanSetting;

	private VView view;
	private VControl control1;
	private VControl control2;
	private DefaultRealm realm;

	@Before
	public void setUp() {
		realm = new DefaultRealm();
		ruleService = new RuleService();
		createView();
		createDomainObject();
	}

	@After
	public void tearDown() {
		realm.dispose();
	}

	private void createDomainObject() {
		fan = BowlingFactory.eINSTANCE.createFan();

		merchandise1 = BowlingFactory.eINSTANCE.createMerchandise();
		merchandise1.setName(M1_RIGHT);
		merchandise1Setting = UniqueSetting.createSetting(merchandise1, BowlingPackage.eINSTANCE.getMerchandise_Name());

		merchandise2 = BowlingFactory.eINSTANCE.createMerchandise();
		merchandise2.setName(M2_RIGHT);
		merchandise2Setting = UniqueSetting.createSetting(merchandise2, BowlingPackage.eINSTANCE.getMerchandise_Name());

		favMerchandise = BowlingFactory.eINSTANCE.createMerchandise();
		favMerchandise.setName(FM_RIGHT);
		favMerchandiseSetting = UniqueSetting.createSetting(favMerchandise,
			BowlingPackage.eINSTANCE.getMerchandise_Name());

		fan.setFavouriteMerchandise(favMerchandise);
		fan.getFanMerchandise().add(merchandise1);
		fan.getFanMerchandise().add(merchandise2);
		fanSetting = UniqueSetting.createSetting(fan, BowlingPackage.eINSTANCE.getFan_FanMerchandise());
	}

	private void createView() {
		view = VViewFactory.eINSTANCE.createView();

		control1 = VViewFactory.eINSTANCE.createControl();
		control1.setDomainModelReference(BowlingPackage.eINSTANCE.getFan_DateOfBirth());

		control2 = VViewFactory.eINSTANCE.createControl();
		control2.setDomainModelReference(BowlingPackage.eINSTANCE.getFan_EMails());

		view.getChildren().add(control1);
		view.getChildren().add(control2);
	}

	private void initialize() {
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, fan, ruleService);
	}

	private static ShowRule addFavMerchandiseRule(VElement element) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();

		final LeafCondition condition = RuleFactory.eINSTANCE.createLeafCondition();

		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		dmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());

		condition.setExpectedValue(FM_RIGHT);
		condition.setDomainModelReference(dmr);
		rule.setCondition(condition);

		element.getAttachments().add(rule);

		return rule;
	}

	private static ShowRule addFanMerchandiseRule(VElement element) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();

		final LeafCondition condition = RuleFactory.eINSTANCE.createLeafCondition();

		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_FanMerchandise());

		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());

		condition.setExpectedValue(M1_RIGHT);
		condition.setDomainModelReference(dmr);
		condition.setValueDomainModelReference(valueDMR);
		rule.setCondition(condition);

		element.getAttachments().add(rule);

		return rule;
	}

	private static ShowRule addFanMerchandiseOrRule(VElement element) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();

		final OrCondition condition = RuleFactory.eINSTANCE.createOrCondition();

		final LeafCondition condition1 = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr1.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_FanMerchandise());
		final VFeaturePathDomainModelReference valueDMR1 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR1.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		condition1.setExpectedValue(M1_RIGHT);
		condition1.setDomainModelReference(dmr1);
		condition1.setValueDomainModelReference(valueDMR1);

		final LeafCondition condition2 = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_FanMerchandise());
		final VFeaturePathDomainModelReference valueDMR2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		condition2.setExpectedValue(M2_RIGHT);
		condition2.setDomainModelReference(dmr2);
		condition2.setValueDomainModelReference(valueDMR2);

		condition.getConditions().add(condition1);
		condition.getConditions().add(condition2);

		rule.setCondition(condition);

		element.getAttachments().add(rule);

		return rule;
	}

	private Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> getSettingsToRules() {
		try {
			final RuleRegistry<ShowRule> ruleRegistry = getRuleRegistry();

			final Field field = ruleRegistry.getClass().getDeclaredField(SETTING_TO_RULES_FIELD_NAME);
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> result = (Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>>) field
				.get(ruleRegistry);
			return result;
		} catch (final SecurityException ex) {
		} catch (final NoSuchFieldException ex) {
		} catch (final IllegalArgumentException ex) {
		} catch (final IllegalAccessException ex) {
		}
		fail(REFLECTION_FAIL_FIX_TEST_SETUP);
		return null;
	}

	private BidirectionalMap<ShowRule, VElement> getRulesToRenderables() {
		try {
			final RuleRegistry<ShowRule> ruleRegistry = getRuleRegistry();

			final Field field = ruleRegistry.getClass().getDeclaredField(RULES_TO_RENDERABLES_FIELD_NAME);
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			final BidirectionalMap<ShowRule, VElement> result = (BidirectionalMap<ShowRule, VElement>) field
				.get(ruleRegistry);
			return result;
		} catch (final SecurityException ex) {
		} catch (final NoSuchFieldException ex) {
		} catch (final IllegalArgumentException ex) {
		} catch (final IllegalAccessException ex) {
		}
		fail(REFLECTION_FAIL_FIX_TEST_SETUP);
		return null;
	}

	private Map<Condition, Set<UniqueSetting>> getConditionsToSettings() {
		try {
			final RuleRegistry<ShowRule> ruleRegistry = getRuleRegistry();

			final Field field = ruleRegistry.getClass().getDeclaredField(CONDITION_TO_SETTINGS_FIELD_NAME);
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			final Map<Condition, Set<UniqueSetting>> result = (Map<Condition, Set<UniqueSetting>>) field
				.get(ruleRegistry);
			return result;
		} catch (final SecurityException ex) {
		} catch (final NoSuchFieldException ex) {
		} catch (final IllegalArgumentException ex) {
		} catch (final IllegalAccessException ex) {
		}
		fail(REFLECTION_FAIL_FIX_TEST_SETUP);
		return null;
	}

	private RuleRegistry<ShowRule> getRuleRegistry() {
		try {
			final Field field = ruleService.getClass().getDeclaredField(SHOW_RULE_REGISTRY_FIELD_NAME);
			field.setAccessible(true);
			@SuppressWarnings("unchecked")
			final RuleRegistry<ShowRule> result = (RuleRegistry<ShowRule>) field.get(ruleService);
			return result;
		} catch (final SecurityException ex) {
		} catch (final NoSuchFieldException ex) {
		} catch (final IllegalArgumentException ex) {
		} catch (final IllegalAccessException ex) {
		}
		fail(REFLECTION_FAIL_FIX_TEST_SETUP);
		return null;
	}

	@Test
	public void testInitOneSingleRefRule() {
		// setup
		final ShowRule rule = addFavMerchandiseRule(control1);

		// act
		initialize();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry = settingsToRules.entrySet()
				.iterator()
				.next();
			assertEquals(favMerchandiseSetting, entry.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules = entry.getValue();
			assertEquals(1, conditionToRules.keys().size());
			assertEquals(1, conditionToRules.values().size());
			final Condition condition = conditionToRules.keys().iterator().next();
			assertEquals(rule.getCondition(), condition);
			assertEquals(rule, conditionToRules.getValue(condition));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(1, entry.getValue().size());
			assertEquals(favMerchandiseSetting, entry.getValue().iterator().next());
		}
	}

	@Test
	public void testInitTwoSimilarSingleRefRule() {
		// setup
		final ShowRule rule1 = addFavMerchandiseRule(control1);
		final ShowRule rule2 = addFavMerchandiseRule(control2);

		// act
		initialize();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry = settingsToRules.entrySet()
				.iterator().next();
			assertEquals(favMerchandiseSetting, entry.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules = entry.getValue();
			assertEquals(2, conditionToRules.keys().size());
			assertEquals(2, conditionToRules.values().size());
			final Iterator<Condition> iterator = conditionToRules.keys().iterator();
			final Condition condition1 = iterator.next();
			assertEquals(rule1.getCondition(), condition1);
			assertEquals(rule1, conditionToRules.getValue(condition1));
			final Condition condition2 = iterator.next();
			assertEquals(rule2.getCondition(), condition2);
			assertEquals(rule2, conditionToRules.getValue(condition2));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(2, rulesToRenderables.keys().size());
			assertEquals(2, rulesToRenderables.values().size());
			final Iterator<ShowRule> iterator = rulesToRenderables.keys().iterator();
			final ShowRule ruleKey1 = iterator.next();
			assertEquals(rule1, ruleKey1);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey1));
			final ShowRule ruleKey2 = iterator.next();
			assertEquals(rule2, ruleKey2);
			assertEquals(control2, rulesToRenderables.getValue(ruleKey2));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(2, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();
			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(rule1.getCondition(), entry1.getKey());
			assertEquals(1, entry1.getValue().size());
			assertEquals(favMerchandiseSetting, entry1.getValue().iterator().next());
			final Entry<Condition, Set<UniqueSetting>> entry2 = iterator.next();
			assertEquals(rule2.getCondition(), entry2.getKey());
			assertEquals(1, entry2.getValue().size());
			assertEquals(favMerchandiseSetting, entry2.getValue().iterator().next());
		}
	}

	@Test
	public void testInitOneMultiRefRule() {
		// setup
		final ShowRule rule = addFanMerchandiseRule(control1);

		// act
		initialize();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(3, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Condition condition2 = conditionToRules2.keys().iterator().next();
			assertEquals(rule.getCondition(), condition2);
			assertEquals(rule, conditionToRules2.getValue(condition2));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(1, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			final Condition condition3 = conditionToRules3.keys().iterator().next();
			assertEquals(rule.getCondition(), condition3);
			assertEquals(rule, conditionToRules3.getValue(condition3));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(3, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(fanSetting, iterator.next());
			assertEquals(merchandise1Setting, iterator.next());
			assertEquals(merchandise2Setting, iterator.next());
		}
	}

	@Test
	public void testInitTwoSimilarMultiRefRule() {
		// setup
		final ShowRule rule1 = addFanMerchandiseRule(control1);
		final ShowRule rule2 = addFanMerchandiseRule(control2);

		// act
		initialize();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(3, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(2, conditionToRules1.keys().size());
			assertEquals(2, conditionToRules1.values().size());
			final Iterator<Condition> conditionToRulesIterator1 = conditionToRules1.keys().iterator();
			final Condition condition11 = conditionToRulesIterator1.next();
			assertEquals(rule1.getCondition(), condition11);
			assertEquals(rule1, conditionToRules1.getValue(condition11));
			final Condition condition12 = conditionToRulesIterator1.next();
			assertEquals(rule2.getCondition(), condition12);
			assertEquals(rule2, conditionToRules1.getValue(condition12));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(2, conditionToRules2.keys().size());
			assertEquals(2, conditionToRules2.values().size());
			final Iterator<Condition> conditionToRulesIterator2 = conditionToRules2.keys().iterator();
			final Condition condition21 = conditionToRulesIterator2.next();
			assertEquals(rule1.getCondition(), condition21);
			assertEquals(rule1, conditionToRules2.getValue(condition21));
			final Condition condition22 = conditionToRulesIterator2.next();
			assertEquals(rule2.getCondition(), condition22);
			assertEquals(rule2, conditionToRules2.getValue(condition22));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(2, conditionToRules3.keys().size());
			assertEquals(2, conditionToRules3.values().size());
			final Iterator<Condition> conditionToRulesIterator3 = conditionToRules3.keys().iterator();
			final Condition condition31 = conditionToRulesIterator3.next();
			assertEquals(rule1.getCondition(), condition31);
			assertEquals(rule1, conditionToRules3.getValue(condition31));
			final Condition condition32 = conditionToRulesIterator3.next();
			assertEquals(rule2.getCondition(), condition32);
			assertEquals(rule2, conditionToRules3.getValue(condition32));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(2, rulesToRenderables.keys().size());
			assertEquals(2, rulesToRenderables.values().size());
			final Iterator<ShowRule> iterator = rulesToRenderables.keys().iterator();

			final ShowRule ruleKey1 = iterator.next();
			assertEquals(rule1, ruleKey1);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey1));

			final ShowRule ruleKey2 = iterator.next();
			assertEquals(rule2, ruleKey2);
			assertEquals(control2, rulesToRenderables.getValue(ruleKey2));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(2, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();

			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(rule1.getCondition(), entry1.getKey());
			assertEquals(3, entry1.getValue().size());
			assertTrue(entry1.getValue().contains(fanSetting));
			assertTrue(entry1.getValue().contains(merchandise1Setting));
			assertTrue(entry1.getValue().contains(merchandise2Setting));

			final Entry<Condition, Set<UniqueSetting>> entry2 = iterator.next();
			assertEquals(rule2.getCondition(), entry2.getKey());
			assertEquals(3, entry2.getValue().size());
			assertTrue(entry2.getValue().contains(fanSetting));
			assertTrue(entry2.getValue().contains(merchandise1Setting));
			assertTrue(entry2.getValue().contains(merchandise2Setting));
		}
	}

	@Test
	public void testInitOrMultiRefRule() {
		// setup
		final ShowRule rule = addFanMerchandiseOrRule(control1);
		final OrCondition orCondition = (OrCondition) rule.getCondition();
		final Condition condition1 = orCondition.getConditions().get(0);
		final Condition condition2 = orCondition.getConditions().get(1);

		// act
		initialize();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(3, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(2, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			assertTrue(conditionToRules1.keys().contains(condition1));
			assertEquals(rule, conditionToRules1.getValue(condition1));
			assertTrue(conditionToRules1.keys().contains(condition2));
			assertEquals(rule, conditionToRules1.getValue(condition2));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(2, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			assertTrue(conditionToRules2.keys().contains(condition1));
			assertEquals(rule, conditionToRules2.getValue(condition1));
			assertTrue(conditionToRules2.keys().contains(condition2));
			assertEquals(rule, conditionToRules2.getValue(condition2));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(2, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			assertTrue(conditionToRules3.keys().contains(condition1));
			assertEquals(rule, conditionToRules3.getValue(condition1));
			assertTrue(conditionToRules3.keys().contains(condition2));
			assertEquals(rule, conditionToRules3.getValue(condition2));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(2, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();

			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(condition1, entry1.getKey());
			assertEquals(3, entry1.getValue().size());
			assertTrue(entry1.getValue().contains(fanSetting));
			assertTrue(entry1.getValue().contains(merchandise1Setting));
			assertTrue(entry1.getValue().contains(merchandise2Setting));

			final Entry<Condition, Set<UniqueSetting>> entry2 = iterator.next();
			assertEquals(condition2, entry2.getKey());
			assertEquals(3, entry2.getValue().size());
			assertTrue(entry2.getValue().contains(fanSetting));
			assertTrue(entry2.getValue().contains(merchandise1Setting));
			assertTrue(entry2.getValue().contains(merchandise2Setting));
		}
	}

	@Test
	public void testInitOneMultiRefRuleEmptyList() {
		// setup
		fan.getFanMerchandise().clear();
		final ShowRule rule = addFanMerchandiseRule(control1);

		// act
		initialize();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(1, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(fanSetting, iterator.next());
		}
	}

	@Test
	public void testDynmicOneSingleRefRuleChangeDomain() {
		// setup
		final ShowRule rule = addFavMerchandiseRule(control1);
		initialize();

		// act
		final Merchandise newMerchandise = BowlingFactory.eINSTANCE.createMerchandise();
		fan.setFavouriteMerchandise(newMerchandise);
		final UniqueSetting newSetting = UniqueSetting.createSetting(newMerchandise,
			BowlingPackage.eINSTANCE.getMerchandise_Name());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry = settingsToRules.entrySet()
				.iterator()
				.next();
			assertEquals(newSetting, entry.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules = entry.getValue();
			assertEquals(1, conditionToRules.keys().size());
			assertEquals(1, conditionToRules.values().size());
			final Condition condition = conditionToRules.keys().iterator().next();
			assertEquals(rule.getCondition(), condition);
			assertEquals(rule, conditionToRules.getValue(condition));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(1, entry.getValue().size());
			assertEquals(newSetting, entry.getValue().iterator().next());
		}
	}

	@Test
	public void testDynamicOneMultiRefRuleChangeDomainAdd() {
		// setup
		final ShowRule rule = addFanMerchandiseRule(control1);
		initialize();

		// act
		final Merchandise newMerchandise = BowlingFactory.eINSTANCE.createMerchandise();
		fan.getFanMerchandise().add(newMerchandise);
		final UniqueSetting newSetting = UniqueSetting.createSetting(newMerchandise,
			BowlingPackage.eINSTANCE.getMerchandise_Name());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(4, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Condition condition2 = conditionToRules2.keys().iterator().next();
			assertEquals(rule.getCondition(), condition2);
			assertEquals(rule, conditionToRules2.getValue(condition2));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(1, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			final Condition condition3 = conditionToRules3.keys().iterator().next();
			assertEquals(rule.getCondition(), condition3);
			assertEquals(rule, conditionToRules3.getValue(condition3));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry4 = iterator.next();
			assertEquals(newSetting, entry4.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules4 = entry4.getValue();
			assertEquals(1, conditionToRules4.keys().size());
			assertEquals(1, conditionToRules4.values().size());
			final Condition condition4 = conditionToRules4.keys().iterator().next();
			assertEquals(rule.getCondition(), condition4);
			assertEquals(rule, conditionToRules4.getValue(condition4));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(4, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(fanSetting, iterator.next());
			assertEquals(merchandise1Setting, iterator.next());
			assertEquals(merchandise2Setting, iterator.next());
			assertEquals(newSetting, iterator.next());
		}
	}

	@Test
	public void testDynamicOneMultiRefRuleChangeDomainRemove() {
		// setup
		final ShowRule rule = addFanMerchandiseRule(control1);
		initialize();

		// act
		fan.getFanMerchandise().remove(1);

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(2, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Condition condition2 = conditionToRules2.keys().iterator().next();
			assertEquals(rule.getCondition(), condition2);
			assertEquals(rule, conditionToRules2.getValue(condition2));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(2, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(fanSetting, iterator.next());
			assertEquals(merchandise1Setting, iterator.next());
		}
	}

	@Test
	public void testDynamicOneMultiRefRuleChangeDomainClear() {
		// setup
		final ShowRule rule = addFanMerchandiseRule(control1);
		initialize();

		// act
		fan.getFanMerchandise().clear();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(1, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(fanSetting, iterator.next());
		}
	}

	@Test
	public void testDynamicOneMultiRefRuleChangeDomainUnaffected() {
		// setup
		final ShowRule rule = addFanMerchandiseRule(control1);
		initialize();

		// act
		fan.getVisitedTournaments().add(BowlingFactory.eINSTANCE.createTournament());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(3, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Condition condition2 = conditionToRules2.keys().iterator().next();
			assertEquals(rule.getCondition(), condition2);
			assertEquals(rule, conditionToRules2.getValue(condition2));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(1, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			final Condition condition3 = conditionToRules3.keys().iterator().next();
			assertEquals(rule.getCondition(), condition3);
			assertEquals(rule, conditionToRules3.getValue(condition3));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(3, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(fanSetting, iterator.next());
			assertEquals(merchandise1Setting, iterator.next());
			assertEquals(merchandise2Setting, iterator.next());
		}
	}

	@Test
	public void testDynamicOneMultiRefRuleChangeDomainAddNonContainment() {
		// setup
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		final LeafCondition condition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_VisitedTournaments());
		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getTournament_PriceMoney());
		condition.setExpectedValue(99.99);
		condition.setDomainModelReference(dmr);
		condition.setValueDomainModelReference(valueDMR);
		rule.setCondition(condition);
		control1.getAttachments().add(rule);

		final Tournament tournament1 = BowlingFactory.eINSTANCE.createTournament();
		fan.getVisitedTournaments().add(tournament1);

		final UniqueSetting newSettingList = UniqueSetting.createSetting(fan,
			BowlingPackage.eINSTANCE.getFan_VisitedTournaments());
		final UniqueSetting newSettingLeaf1 = UniqueSetting.createSetting(tournament1,
			BowlingPackage.eINSTANCE.getTournament_PriceMoney());

		initialize();

		// act
		final Tournament tournament2 = BowlingFactory.eINSTANCE.createTournament();
		fan.getVisitedTournaments().add(tournament2);
		final UniqueSetting newSettingLeaf2 = UniqueSetting.createSetting(tournament2,
			BowlingPackage.eINSTANCE.getTournament_PriceMoney());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(3, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(newSettingList, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(newSettingLeaf1, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Condition condition2 = conditionToRules2.keys().iterator().next();
			assertEquals(rule.getCondition(), condition2);
			assertEquals(rule, conditionToRules2.getValue(condition2));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(newSettingLeaf2, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(1, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			final Condition condition3 = conditionToRules3.keys().iterator().next();
			assertEquals(rule.getCondition(), condition3);
			assertEquals(rule, conditionToRules3.getValue(condition3));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(3, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(newSettingList, iterator.next());
			assertEquals(newSettingLeaf1, iterator.next());
			assertEquals(newSettingLeaf2, iterator.next());
		}
	}

	@Test
	public void testDynamicOneSingleRefRuleRemoveRule() {
		// setup
		addFavMerchandiseRule(control1);
		initialize();

		// act
		control1.getAttachments().clear();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(0, settingsToRules.size());
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(0, rulesToRenderables.keys().size());
			assertEquals(0, rulesToRenderables.values().size());
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(0, conditionsToSettings.size());
		}
	}

	@Test
	public void testDynamicTwoSimilarSingleRefRuleRemoveOneRule() {
		// setup
		final ShowRule rule1 = addFavMerchandiseRule(control1);
		addFavMerchandiseRule(control2);
		initialize();

		// act
		control2.getAttachments().clear();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry = settingsToRules.entrySet()
				.iterator().next();
			assertEquals(favMerchandiseSetting, entry.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules = entry.getValue();
			assertEquals(1, conditionToRules.keys().size());
			assertEquals(1, conditionToRules.values().size());
			final Iterator<Condition> iterator = conditionToRules.keys().iterator();
			final Condition condition1 = iterator.next();
			assertEquals(rule1.getCondition(), condition1);
			assertEquals(rule1, conditionToRules.getValue(condition1));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final Iterator<ShowRule> iterator = rulesToRenderables.keys().iterator();
			final ShowRule ruleKey1 = iterator.next();
			assertEquals(rule1, ruleKey1);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey1));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();
			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(rule1.getCondition(), entry1.getKey());
			assertEquals(1, entry1.getValue().size());
			assertEquals(favMerchandiseSetting, entry1.getValue().iterator().next());
		}
	}

	@Test
	public void testDynmicOneMultiRefRuleRemoveRule() {
		// setup
		addFanMerchandiseRule(control1);
		initialize();

		// act
		control1.getAttachments().clear();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(0, settingsToRules.size());
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(0, rulesToRenderables.keys().size());
			assertEquals(0, rulesToRenderables.values().size());
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(0, conditionsToSettings.size());
		}
	}

	@Test
	public void testDynamicTwoSimilarMultiRefRuleRemoveOneRule() {
		// setup
		final ShowRule rule1 = addFanMerchandiseRule(control1);
		addFanMerchandiseRule(control2);
		initialize();

		// act
		control2.getAttachments().clear();

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(3, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Iterator<Condition> conditionToRulesIterator1 = conditionToRules1.keys().iterator();
			final Condition condition11 = conditionToRulesIterator1.next();
			assertEquals(rule1.getCondition(), condition11);
			assertEquals(rule1, conditionToRules1.getValue(condition11));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Iterator<Condition> conditionToRulesIterator2 = conditionToRules2.keys().iterator();
			final Condition condition21 = conditionToRulesIterator2.next();
			assertEquals(rule1.getCondition(), condition21);
			assertEquals(rule1, conditionToRules2.getValue(condition21));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(1, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			final Iterator<Condition> conditionToRulesIterator3 = conditionToRules3.keys().iterator();
			final Condition condition31 = conditionToRulesIterator3.next();
			assertEquals(rule1.getCondition(), condition31);
			assertEquals(rule1, conditionToRules3.getValue(condition31));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final Iterator<ShowRule> iterator = rulesToRenderables.keys().iterator();

			final ShowRule ruleKey1 = iterator.next();
			assertEquals(rule1, ruleKey1);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey1));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();

			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(rule1.getCondition(), entry1.getKey());
			assertEquals(3, entry1.getValue().size());
			assertTrue(entry1.getValue().contains(fanSetting));
			assertTrue(entry1.getValue().contains(merchandise1Setting));
			assertTrue(entry1.getValue().contains(merchandise2Setting));
		}
	}

	@Test
	public void testDynmicOneSingleRefRuleChangeCondition() {
		// setup
		final ShowRule rule = addFavMerchandiseRule(control1);
		initialize();

		// act
		final LeafCondition leafCondition = (LeafCondition) rule.getCondition();
		final VFeaturePathDomainModelReference reference = (VFeaturePathDomainModelReference) leafCondition
			.getDomainModelReference();
		reference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_SerialNumber());
		leafCondition.setExpectedValue(1234567890);
		final UniqueSetting newSetting = UniqueSetting.createSetting(favMerchandise,
			BowlingPackage.eINSTANCE.getMerchandise_SerialNumber());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(1, settingsToRules.size());
			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry = settingsToRules.entrySet()
				.iterator()
				.next();
			assertEquals(newSetting, entry.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules = entry.getValue();
			assertEquals(1, conditionToRules.keys().size());
			assertEquals(1, conditionToRules.values().size());
			final Condition condition = conditionToRules.keys().iterator().next();
			assertEquals(rule.getCondition(), condition);
			assertEquals(rule, conditionToRules.getValue(condition));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(1, entry.getValue().size());
			assertEquals(newSetting, entry.getValue().iterator().next());
		}
	}

	@Test
	public void testDynamicTwoSimilarSingleRefRuleChangeOneCondition() {
		// setup
		final ShowRule rule1 = addFavMerchandiseRule(control1);
		final ShowRule rule2 = addFavMerchandiseRule(control2);
		initialize();

		// act
		final LeafCondition leafCondition = (LeafCondition) rule2.getCondition();
		final VFeaturePathDomainModelReference reference = (VFeaturePathDomainModelReference) leafCondition
			.getDomainModelReference();
		reference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_SerialNumber());
		leafCondition.setExpectedValue(1234567890);
		final UniqueSetting newSetting = UniqueSetting.createSetting(favMerchandise,
			BowlingPackage.eINSTANCE.getMerchandise_SerialNumber());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(2, settingsToRules.size());

			assertTrue(settingsToRules.containsKey(favMerchandiseSetting));
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = settingsToRules.get(favMerchandiseSetting);
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Iterator<Condition> iterator1 = conditionToRules1.keys().iterator();
			final Condition condition1 = iterator1.next();
			assertEquals(rule1.getCondition(), condition1);
			assertEquals(rule1, conditionToRules1.getValue(condition1));

			assertTrue(settingsToRules.containsKey(newSetting));
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = settingsToRules.get(newSetting);
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Iterator<Condition> iterator2 = conditionToRules2.keys().iterator();
			final Condition condition2 = iterator2.next();
			assertEquals(rule2.getCondition(), condition2);
			assertEquals(rule2, conditionToRules2.getValue(condition2));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(2, rulesToRenderables.keys().size());
			assertEquals(2, rulesToRenderables.values().size());
			final Iterator<ShowRule> iterator = rulesToRenderables.keys().iterator();
			final ShowRule ruleKey1 = iterator.next();
			assertEquals(rule1, ruleKey1);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey1));
			final ShowRule ruleKey2 = iterator.next();
			assertEquals(rule2, ruleKey2);
			assertEquals(control2, rulesToRenderables.getValue(ruleKey2));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(2, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();
			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(rule1.getCondition(), entry1.getKey());
			assertEquals(1, entry1.getValue().size());
			assertEquals(favMerchandiseSetting, entry1.getValue().iterator().next());
			final Entry<Condition, Set<UniqueSetting>> entry2 = iterator.next();
			assertEquals(rule2.getCondition(), entry2.getKey());
			assertEquals(1, entry2.getValue().size());
			assertEquals(newSetting, entry2.getValue().iterator().next());
		}
	}

	@Test
	public void testDynamicOneMultiRefRuleChangeConditionToOtherMultiRef() {
		// setup
		final ShowRule rule = addFanMerchandiseRule(control1);
		initialize();

		// act
		final Tournament tournament = BowlingFactory.eINSTANCE.createTournament();
		fan.getVisitedTournaments().add(tournament);

		final LeafCondition leafCondition = (LeafCondition) rule.getCondition();
		final VFeaturePathDomainModelReference reference = (VFeaturePathDomainModelReference) leafCondition
			.getDomainModelReference();
		reference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_VisitedTournaments());
		final VFeaturePathDomainModelReference valueDMR = (VFeaturePathDomainModelReference) leafCondition
			.getValueDomainModelReference();
		valueDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getTournament_PriceMoney());
		leafCondition.setExpectedValue(99.99);

		final UniqueSetting newSettingList = UniqueSetting.createSetting(fan,
			BowlingPackage.eINSTANCE.getFan_VisitedTournaments());
		final UniqueSetting newSettingLeaf = UniqueSetting.createSetting(tournament,
			BowlingPackage.eINSTANCE.getTournament_PriceMoney());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(2, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(newSettingList, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Condition condition1 = conditionToRules1.keys().iterator().next();
			assertEquals(rule.getCondition(), condition1);
			assertEquals(rule, conditionToRules1.getValue(condition1));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(newSettingLeaf, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Condition condition2 = conditionToRules2.keys().iterator().next();
			assertEquals(rule.getCondition(), condition2);
			assertEquals(rule, conditionToRules2.getValue(condition2));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(1, rulesToRenderables.keys().size());
			assertEquals(1, rulesToRenderables.values().size());
			final ShowRule ruleKey = rulesToRenderables.keys().iterator().next();
			assertEquals(rule, ruleKey);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(1, conditionsToSettings.size());
			final Entry<Condition, Set<UniqueSetting>> entry = conditionsToSettings.entrySet().iterator().next();
			assertEquals(rule.getCondition(), entry.getKey());
			assertEquals(2, entry.getValue().size());
			final Iterator<UniqueSetting> iterator = entry.getValue().iterator();
			assertEquals(newSettingList, iterator.next());
			assertEquals(newSettingLeaf, iterator.next());
		}
	}

	@Test
	public void testDynamicTwoSimilarMultiRefRuleChangeOneConditionNoNonMulti() {
		// setup
		final ShowRule rule1 = addFanMerchandiseRule(control1);
		final ShowRule rule2 = addFanMerchandiseRule(control2);
		initialize();

		// act
		final LeafCondition leafCondition = (LeafCondition) rule2.getCondition();
		final VFeaturePathDomainModelReference reference = (VFeaturePathDomainModelReference) leafCondition
			.getDomainModelReference();
		reference.getDomainModelEReferencePath().clear();
		reference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		reference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_SerialNumber());
		leafCondition.setExpectedValue(1234567890);
		final UniqueSetting newSetting = UniqueSetting.createSetting(favMerchandise,
			BowlingPackage.eINSTANCE.getMerchandise_SerialNumber());

		// assert settingsToRules
		{
			final Map<UniqueSetting, BidirectionalMap<Condition, ShowRule>> settingsToRules = getSettingsToRules();
			assertEquals(4, settingsToRules.size());
			final Iterator<Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>>> iterator = settingsToRules
				.entrySet().iterator();

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry1 = iterator.next();
			assertEquals(fanSetting, entry1.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules1 = entry1.getValue();
			assertEquals(1, conditionToRules1.keys().size());
			assertEquals(1, conditionToRules1.values().size());
			final Iterator<Condition> conditionToRulesIterator1 = conditionToRules1.keys().iterator();
			final Condition condition11 = conditionToRulesIterator1.next();
			assertEquals(rule1.getCondition(), condition11);
			assertEquals(rule1, conditionToRules1.getValue(condition11));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry2 = iterator.next();
			assertEquals(merchandise1Setting, entry2.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules2 = entry2.getValue();
			assertEquals(1, conditionToRules2.keys().size());
			assertEquals(1, conditionToRules2.values().size());
			final Iterator<Condition> conditionToRulesIterator2 = conditionToRules2.keys().iterator();
			final Condition condition21 = conditionToRulesIterator2.next();
			assertEquals(rule1.getCondition(), condition21);
			assertEquals(rule1, conditionToRules2.getValue(condition21));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry3 = iterator.next();
			assertEquals(merchandise2Setting, entry3.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules3 = entry3.getValue();
			assertEquals(1, conditionToRules3.keys().size());
			assertEquals(1, conditionToRules3.values().size());
			final Iterator<Condition> conditionToRulesIterator3 = conditionToRules3.keys().iterator();
			final Condition condition31 = conditionToRulesIterator3.next();
			assertEquals(rule1.getCondition(), condition31);
			assertEquals(rule1, conditionToRules3.getValue(condition31));

			final Entry<UniqueSetting, BidirectionalMap<Condition, ShowRule>> entry4 = iterator.next();
			assertEquals(newSetting, entry4.getKey());
			final BidirectionalMap<Condition, ShowRule> conditionToRules4 = entry4.getValue();
			assertEquals(1, conditionToRules4.keys().size());
			assertEquals(1, conditionToRules4.values().size());
			final Iterator<Condition> conditionToRulesIterator4 = conditionToRules4.keys().iterator();
			final Condition condition41 = conditionToRulesIterator4.next();
			assertEquals(rule2.getCondition(), condition41);
			assertEquals(rule2, conditionToRules4.getValue(condition41));
		}

		// assert rulesToRenderables
		{
			final BidirectionalMap<ShowRule, VElement> rulesToRenderables = getRulesToRenderables();
			assertEquals(2, rulesToRenderables.keys().size());
			assertEquals(2, rulesToRenderables.values().size());
			final Iterator<ShowRule> iterator = rulesToRenderables.keys().iterator();

			final ShowRule ruleKey1 = iterator.next();
			assertEquals(rule1, ruleKey1);
			assertEquals(control1, rulesToRenderables.getValue(ruleKey1));

			final ShowRule ruleKey2 = iterator.next();
			assertEquals(rule2, ruleKey2);
			assertEquals(control2, rulesToRenderables.getValue(ruleKey2));
		}

		// assert conditionToSettings
		{
			final Map<Condition, Set<UniqueSetting>> conditionsToSettings = getConditionsToSettings();
			assertEquals(2, conditionsToSettings.size());
			final Iterator<Entry<Condition, Set<UniqueSetting>>> iterator = conditionsToSettings.entrySet().iterator();

			final Entry<Condition, Set<UniqueSetting>> entry1 = iterator.next();
			assertEquals(rule1.getCondition(), entry1.getKey());
			assertEquals(3, entry1.getValue().size());
			assertTrue(entry1.getValue().contains(fanSetting));
			assertTrue(entry1.getValue().contains(merchandise1Setting));
			assertTrue(entry1.getValue().contains(merchandise2Setting));

			final Entry<Condition, Set<UniqueSetting>> entry2 = iterator.next();
			assertEquals(rule2.getCondition(), entry2.getKey());
			assertEquals(1, entry2.getValue().size());
			assertTrue(entry2.getValue().contains(newSetting));
		}
	}

}// END COMPLEX CODE
