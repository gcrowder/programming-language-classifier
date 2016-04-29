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
package org.eclipse.emf.ecp.view.rule.ui.swt.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.rule.test.RuleHandle;
import org.eclipse.emf.ecp.view.rule.test.RuleTestHelper;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.spi.rule.model.Rule;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Jonas
 *
 */
@RunWith(DatabindingClassRunner.class)
public class RuleSWT_PTest {

	private org.eclipse.emf.ecp.view.spi.model.VView view;
	private org.eclipse.emf.ecp.view.spi.model.VControl control;
	private Shell shell;
	private EObject input;
	private org.eclipse.swt.widgets.Control renderedControl;

	@Before
	public void init() {
		view = createView();
		input = createFan();
		control = (VControl) view.getChildren().get(0);

		final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		modelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		control.setDomainModelReference(modelReference);

		shell = SWTViewTestHelper.createShell();
		shell.setVisible(true);
	}

	@Test
	public void testEnableRuleWithFalse() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		addEnableRule();
		render();
		assertTrue(isControlEnabled());
	}

	@Test
	public void testDisableRuleWithFalse() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		addDisableRule();
		render();
		assertTrue(isControlEnabled());
	}

	@Test
	public void testVisibleRuleWithFalse() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		addVisibleRule();
		render();
		assertTrue(isControlVisible());
	}

	@Test
	public void testInVisibleRuleWithFalse() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		addInVisibleRule();
		render();
		assertTrue(isControlVisible());
	}

	@Test
	public void testDisableRuleAndTrueLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addDisableRule();
		RuleTestHelper.addTrueLeafCondition(ruleHandle.getRule());
		render();
		assertFalse(isControlEnabled());
	}

	@Test
	public void testDisableRuleAndFalseLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addDisableRule();
		RuleTestHelper.addFalseLeafCondition(ruleHandle.getRule());
		render();
		assertTrue(isControlEnabled());
	}

	@Test
	public void testEnableRuleAndFalseLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addEnableRule();
		RuleTestHelper.addFalseLeafCondition(ruleHandle.getRule());
		render();
		assertFalse(isControlEnabled());
	}

	@Test
	public void testEnabledRuleAndTrueLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addEnableRule();
		RuleTestHelper.addTrueLeafCondition(ruleHandle.getRule());
		render();
		assertTrue(isControlEnabled());
	}

	@Test
	public void testInvisibleRuleAndFalseLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addInVisibleRule();
		RuleTestHelper.addFalseLeafCondition(ruleHandle.getRule());
		render();
		assertTrue(isControlVisible());
	}

	@Test
	public void testInvisibleRuleAndTrueLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addInVisibleRule();
		RuleTestHelper.addTrueLeafCondition(ruleHandle.getRule());
		render();
		assertFalse(isControlVisible());
	}

	@Test
	public void testVisibleRuleAndTrueLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addVisibleRule();
		RuleTestHelper.addTrueLeafCondition(ruleHandle.getRule());
		render();
		assertTrue(isControlVisible());
	}

	@Test
	public void testVisibleRuleAndFalseLeafCondition() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final RuleHandle ruleHandle = addVisibleRule();
		RuleTestHelper.addFalseLeafCondition(ruleHandle.getRule());
		render();
		assertFalse(isControlVisible());
	}

	/**
	 * @return
	 *
	 */
	private RuleHandle addInVisibleRule() {
		final RuleHandle invisibleShowRule = RuleTestHelper.createInvisibleShowRule();
		addRuleToElement(invisibleShowRule.getRule(), view);
		return invisibleShowRule;

	}

	/**
	 * @return
	 */
	private boolean isControlVisible() {
		final org.eclipse.swt.widgets.Control control = getControlWhichIsEnabled(renderedControl);
		return control.isVisible();
	}

	/**
	 *
	 */
	private RuleHandle addVisibleRule() {
		final RuleHandle visibleShowRule = RuleTestHelper.createVisibleShowRule();
		addRuleToElement(visibleShowRule.getRule(), view);
		return visibleShowRule;

	}

	private RuleHandle addDisableRule() {
		final RuleHandle enabledEnableRule = RuleTestHelper.createDisabledEnableRule();
		addRuleToElement(enabledEnableRule.getRule(), view);
		return enabledEnableRule;
	}

	/**
	 * @throws NoPropertyDescriptorFoundExeption
	 * @throws NoRendererFoundException
	 * @throws EMFFormsNoRendererException
	 *
	 */
	private void render() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		renderedControl = SWTViewTestHelper.render(view, input, shell);

	}

	/**
	 * @return
	 */
	private boolean isControlEnabled() {
		final org.eclipse.swt.widgets.Control control = getControlWhichIsEnabled(renderedControl);
		return control.isEnabled();
	}

	/**
	 * @param control
	 */
	private org.eclipse.swt.widgets.Control getControlWhichIsEnabled(org.eclipse.swt.widgets.Control control) {
		assertTrue(control instanceof Composite);

		return control;
	}

	/**
	 * @param control
	 */
	private RuleHandle addEnableRule() {
		final RuleHandle enabledEnableRule = RuleTestHelper.createEnabledEnableRule();
		addRuleToElement(enabledEnableRule.getRule(), view);
		return enabledEnableRule;
	}

	/**
	 * @param enabledEnableRule
	 * @param control2
	 */
	private void addRuleToElement(Rule enabledEnableRule, VElement renderable) {
		renderable.getAttachments().add(enabledEnableRule);
	}

	private Fan createFan() {
		final Fan fan = BowlingFactory.eINSTANCE.createFan();
		final Merchandise merchandise = BowlingFactory.eINSTANCE.createMerchandise();
		fan.setFavouriteMerchandise(merchandise);
		merchandise.setName("foo");
		return fan;
	}

	private org.eclipse.emf.ecp.view.spi.model.VView createView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		return view;
	}
}
