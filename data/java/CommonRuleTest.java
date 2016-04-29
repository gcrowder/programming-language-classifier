/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import java.util.Arrays;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.AndCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.Condition;
import org.eclipse.emf.ecp.view.spi.rule.model.EnableRule;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.OrCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;

/**
 * @author Edgar
 *
 */
public class CommonRuleTest {

	/**
	 * Adds the league show rule.
	 *
	 * @param control the control
	 * @param showOnRightValue the visible on right value
	 */
	protected void addLeagueShowRule(VElement control, boolean showOnRightValue) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(!showOnRightValue);
		rule.setCondition(createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"));
		control.getAttachments().add(rule);
	}

	/**
	 * Adds a {@link ShowRule} to the given {@link VElement} and allows specifying
	 * the condition to be fulfilled.
	 *
	 * @param control
	 *            the {@link VElement} to which the show rule should be attached
	 * @param showOnRightValue
	 *            whether the show rule should cause the {@link VElement} to be
	 *            hidden or show if the condition is fulfilled
	 * @param attribute
	 *            the attribute the condition is pointing to
	 * @param expectedValue
	 *            the expected value of the attribute
	 */
	protected void addShowRule(VElement control, boolean showOnRightValue, EAttribute attribute,
		Object expectedValue) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(!showOnRightValue);
		rule.setCondition(createLeafCondition(attribute, expectedValue));
		control.getAttachments().add(rule);
	}

	/**
	 * Adds a {@link ShowRule} to the given {@link VElement} without a condition.
	 */
	protected ShowRule addShowRule(VElement control, boolean isHide) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(isHide);
		control.getAttachments().add(rule);
		return rule;
	}

	/**
	 * Adds a {@link EnableRule} to the given {@link VElement} without a condition.
	 */
	protected void addEnableRule(VElement control, boolean isDisabled) {
		final EnableRule rule = RuleFactory.eINSTANCE.createEnableRule();
		rule.setDisable(isDisabled);
		control.getAttachments().add(rule);
	}

	protected void addEnableRule(VElement control, boolean enableOnRightValue, EAttribute attribute,
		Object expectedValue) {
		final EnableRule rule = RuleFactory.eINSTANCE.createEnableRule();
		rule.setDisable(!enableOnRightValue);
		rule.setCondition(createLeafCondition(attribute, expectedValue));
		control.getAttachments().add(rule);
	}

	protected LeafCondition createLeafCondition(EAttribute attribute, Object expectedValue, EReference... eReferences) {
		final LeafCondition condition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelReference.setDomainModelEFeature(attribute);
		modelReference.getDomainModelEReferencePath().addAll(Arrays.asList(eReferences));
		condition.setDomainModelReference(modelReference);
		condition.setExpectedValue(expectedValue);
		return condition;
	}

	protected void addLeagueShowRuleWithOrCondition(VElement control, boolean hideOnRightValue,
		Condition... childConditions) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(!hideOnRightValue);
		final OrCondition condition = RuleFactory.eINSTANCE.createOrCondition();
		for (final Condition childCondition : childConditions) {
			condition.getConditions().add(childCondition);
		}
		rule.setCondition(condition);
		control.getAttachments().add(rule);
	}

	protected void addLeagueEnableRuleWithOrCondition(VElement control, boolean disableOnRightValue,
		Condition... childConditions) {
		final EnableRule rule = RuleFactory.eINSTANCE.createEnableRule();
		rule.setDisable(!disableOnRightValue);
		final OrCondition condition = RuleFactory.eINSTANCE.createOrCondition();
		for (final Condition childCondition : childConditions) {
			condition.getConditions().add(childCondition);
		}
		rule.setCondition(condition);
		control.getAttachments().add(rule);
	}

	protected void addLeagueShowRuleWithAndCondition(VElement control, boolean hideOnRightValue,
		Condition... childConditions) {
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(!hideOnRightValue);
		final AndCondition condition = RuleFactory.eINSTANCE.createAndCondition();
		for (final Condition childCondition : childConditions) {
			condition.getConditions().add(childCondition);
		}
		rule.setCondition(condition);
		control.getAttachments().add(rule);
	}

	protected void addLeagueEnableRuleWithAndCondition(VElement control, boolean disableOnRightValue,
		Condition... childConditions) {
		final EnableRule rule = RuleFactory.eINSTANCE.createEnableRule();
		rule.setDisable(!disableOnRightValue);
		final AndCondition condition = RuleFactory.eINSTANCE.createAndCondition();
		for (final Condition childCondition : childConditions) {
			condition.getConditions().add(childCondition);
		}
		rule.setCondition(condition);
		control.getAttachments().add(rule);
	}

	/**
	 * Adds the league enable rule.
	 *
	 * @param control the control
	 * @param enableOnRightValue the enable on right value
	 */
	protected void addLeagueEnableRule(VElement control, boolean enableOnRightValue) {
		final EnableRule rule = RuleFactory.eINSTANCE.createEnableRule();
		rule.setDisable(!enableOnRightValue);
		final LeafCondition condition = RuleFactory.eINSTANCE.createLeafCondition();
		rule.setCondition(condition);
		final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Name());
		condition.setDomainModelReference(modelReference);
		condition.setExpectedValue("League");
		control.getAttachments().add(rule);
	}

}
