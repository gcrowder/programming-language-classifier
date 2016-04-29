/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.EnableRule;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.Rule;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;

/**
 * Tests Rule Evaluation and its impact on node with conditions.
 *
 * @author jonas
 */
public final class RuleTestHelper {

	private RuleTestHelper() {
		// util
	}

	public static void addFalseLeafCondition(Rule rule) {
		final LeafCondition leafCondition = createFalseLeafCondition();
		rule.setCondition(leafCondition);
	}

	private static LeafCondition createFalseLeafCondition() {
		final LeafCondition leafCondition = createLeafCondition();
		leafCondition.setExpectedValue("bar");
		return leafCondition;
	}

	private static LeafCondition createTrueLeafCondition() {
		final LeafCondition leafCondition = createLeafCondition();
		leafCondition.setExpectedValue("foo");
		return leafCondition;
	}

	public static void addTrueLeafCondition(Rule rule) {
		final LeafCondition leafCondition = createTrueLeafCondition();
		rule.setCondition(leafCondition);
	}

	private static LeafCondition createLeafCondition() {
		final LeafCondition leafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		modelReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		leafCondition.setDomainModelReference(modelReference);

		return leafCondition;
	}

	public static RuleHandle createDisabledEnableRule() {
		final EnableRule enableRule = createEnableRule();
		enableRule.setDisable(true);
		return createRuleHandle(enableRule);
	}

	public static RuleHandle createEnabledEnableRule() {
		final EnableRule enableRule = createEnableRule();
		enableRule.setDisable(false);
		return createRuleHandle(enableRule);
	}

	private static RuleHandle createRuleHandle(Rule enableRule) {
		final VElement renderable = createRuleContainerAndAddRule(enableRule);
		final Fan fan = BowlingFactory.eINSTANCE.createFan();
		final Merchandise merchandise = BowlingFactory.eINSTANCE.createMerchandise();
		merchandise.setName("foo");
		fan.setFavouriteMerchandise(merchandise);
		return new RuleHandle(enableRule, renderable, fan);
	}

	private static VElement createRuleContainerAndAddRule(Rule rule) {
		final VElement renderable = VViewFactory.eINSTANCE.createView();
		renderable.getAttachments().add(rule);
		return renderable;
	}

	private static EnableRule createEnableRule() {
		return RuleFactory.eINSTANCE.createEnableRule();
	}

	public static RuleHandle createVisibleShowRule() {
		final ShowRule showRule = RuleFactory.eINSTANCE.createShowRule();
		showRule.setHide(false);
		return createRuleHandle(showRule);
	}

	public static RuleHandle createInvisibleShowRule() {
		final ShowRule showRule = RuleFactory.eINSTANCE.createShowRule();
		showRule.setHide(true);
		return createRuleHandle(showRule);
	}

}
