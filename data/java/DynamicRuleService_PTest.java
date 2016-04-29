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
package org.eclipse.emf.ecp.view.rule.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.NoSuchElementException;

import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.internal.rule.RuleService;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VContainedContainer;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.EnableRule;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.OrCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Eugen
 * @author jfaltermeier
 *
 */
public class DynamicRuleService_PTest {

	private static final double HEIGHT = 2;
	private static final double HEIGHT_ALT = 3;
	private static final double PLAYER_HEIGHT = 1;

	private RuleService rs;
	private VView view;
	private Player domainObject;
	private DefaultRealm realm;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		realm = new DefaultRealm();
		rs = new RuleService();
		domainObject = createDomainObject();
	}

	@After
	public void tearDown() {
		realm.dispose();
	}

	private void initialize() {
		ViewModelContextFactory.INSTANCE.createViewModelContext(view, domainObject,
			rs);
	}

	private Player createDomainObject() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		player.setName("Test");
		player.setHeight(PLAYER_HEIGHT);
		return player;
	}

	private ShowRule createShowRule(double expectedValue) {
		final ShowRule showRule = RuleFactory.eINSTANCE.createShowRule();
		final LeafCondition lc = RuleFactory.eINSTANCE.createLeafCondition();
		showRule.setCondition(lc);
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		lc.setDomainModelReference(dmr);
		lc.setExpectedValue(expectedValue);
		return showRule;
	}

	private EnableRule createEnableRule(double expectedValue) {
		final EnableRule enabelRule = RuleFactory.eINSTANCE.createEnableRule();
		final LeafCondition lc = RuleFactory.eINSTANCE.createLeafCondition();
		enabelRule.setCondition(lc);
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		lc.setDomainModelReference(dmr);
		lc.setExpectedValue(expectedValue);
		return enabelRule;
	}

	private EnableRule createEnableRule(double expectedValue1, double expectedValue2) {
		final EnableRule enableRule = RuleFactory.eINSTANCE.createEnableRule();

		final OrCondition oc = RuleFactory.eINSTANCE.createOrCondition();

		final LeafCondition lc1 = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr1.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		lc1.setDomainModelReference(dmr1);
		lc1.setExpectedValue(expectedValue1);
		oc.getConditions().add(lc1);

		final LeafCondition lc2 = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		lc2.setDomainModelReference(dmr2);
		lc2.setExpectedValue(expectedValue2);
		oc.getConditions().add(lc2);

		enableRule.setCondition(oc);

		return enableRule;
	}

	private VView createSimpleView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VControl controlName = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference dmrName = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmrName.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		controlName.setDomainModelReference(dmrName);
		view.getChildren().add(controlName);
		return view;
	}

	private VView createContainerView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		final VVerticalLayout verticalLayout = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(verticalLayout);
		final VControl controlName = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference dmrName = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmrName.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		controlName.setDomainModelReference(dmrName);
		verticalLayout.getChildren().add(controlName);
		return view;
	}

	private VVerticalLayout getVerticalLayout(VView view) {
		for (final VContainedElement element : view.getChildren()) {
			if (element instanceof VVerticalLayout) {
				return (VVerticalLayout) element;
			}
		}
		throw new NoSuchElementException("No vertical layout found in the view's children.");
	}

	@Test
	public void testInitEmptyShowRuleOnControl() {
		view = createSimpleView();
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		view.getChildren().get(0).getAttachments().add(sr);
		initialize();

		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testInitEmptyShowRuleOppositeOnControl() {
		view = createSimpleView();
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		sr.setHide(true);
		view.getChildren().get(0).getAttachments().add(sr);
		initialize();

		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testInitEmptyConditionOnControl() {
		view = createSimpleView();
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		final LeafCondition lc = RuleFactory.eINSTANCE.createLeafCondition();
		sr.setCondition(lc);
		view.getChildren().get(0).getAttachments().add(sr);
		initialize();

		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testInitEmptyDMROnControl() {
		view = createSimpleView();
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		final LeafCondition lc = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		lc.setDomainModelReference(dmr);
		sr.setCondition(lc);
		view.getChildren().get(0).getAttachments().add(sr);
		initialize();

		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testUnsetDMROnControl() {
		view = createSimpleView();
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		final LeafCondition lc = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		lc.setDomainModelReference(dmr);
		sr.setCondition(lc);
		view.getChildren().get(0).getAttachments().add(sr);
		initialize();

		assertFalse(view.getChildren().get(0).isVisible());

		dmr.setDomainModelEFeature(null);

		assertTrue(view.getChildren().get(0).isVisible());

	}

	@Test
	public void testAddEmptyShowRuleOnControl() {
		view = createSimpleView();
		initialize();

		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		view.getChildren().get(0).getAttachments().add(sr);
		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEmptyConditionOnControl() {
		view = createSimpleView();
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		view.getChildren().get(0).getAttachments().add(sr);
		initialize();
		final LeafCondition lc = RuleFactory.eINSTANCE.createLeafCondition();
		sr.setCondition(lc);
		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEmptyShowRuleOppositeOnControl() {
		view = createSimpleView();
		initialize();

		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		sr.setHide(true);
		view.getChildren().get(0).getAttachments().add(sr);
		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddAndRemoveEmptyShowRuleOnControl() {
		view = createSimpleView();
		initialize();

		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		view.getChildren().get(0).getAttachments().add(sr);
		assertTrue(view.getChildren().get(0).isVisible());
		view.getChildren().get(0).getAttachments().remove(sr);
		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEmptyEnableRuleOnControl() {
		view = createSimpleView();
		initialize();

		final EnableRule er = RuleFactory.eINSTANCE.createEnableRule();
		view.getChildren().get(0).getAttachments().add(er);
		assertTrue(view.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddAndRemoveEmptyEnableRuleOnControl() {
		view = createSimpleView();
		initialize();

		final EnableRule er = RuleFactory.eINSTANCE.createEnableRule();
		view.getChildren().get(0).getAttachments().add(er);
		assertTrue(view.getChildren().get(0).isEnabled());
		view.getChildren().get(0).getAttachments().remove(er);
		assertTrue(view.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddEmptyShowRuleOnContainer() {
		view = createContainerView();
		initialize();

		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		view.getChildren().get(0).getAttachments().add(sr);
		assertTrue(view.getChildren().get(0).isVisible());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
	}

	@Test
	public void testAddAndRemoveEmptyShowRuleOnContainer() {
		view = createContainerView();
		initialize();

		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		view.getChildren().get(0).getAttachments().add(sr);
		assertTrue(view.getChildren().get(0).isVisible());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
		view.getChildren().get(0).getAttachments().remove(sr);
		assertTrue(view.getChildren().get(0).isVisible());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEmptyEnableRuleOnContainer() {
		view = createContainerView();
		initialize();

		final EnableRule er = RuleFactory.eINSTANCE.createEnableRule();
		view.getChildren().get(0).getAttachments().add(er);
		assertTrue(view.getChildren().get(0).isEnabled());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddAndRemoveEmptyEnableRuleOnContainer() {
		view = createContainerView();
		initialize();

		final EnableRule er = RuleFactory.eINSTANCE.createEnableRule();
		view.getChildren().get(0).getAttachments().add(er);
		assertTrue(view.getChildren().get(0).isEnabled());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isEnabled());
		view.getChildren().get(0).getAttachments().remove(er);
		assertTrue(view.getChildren().get(0).isEnabled());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddEmptyShowRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		final ShowRule sr2 = RuleFactory.eINSTANCE.createShowRule();
		container.getAttachments().add(sr);
		container.getChildren().get(0).getAttachments().add(sr2);

		assertTrue(container.isVisible());
		assertTrue(container.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddAndRemoveEmptyShowRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final ShowRule sr = RuleFactory.eINSTANCE.createShowRule();
		final ShowRule sr2 = RuleFactory.eINSTANCE.createShowRule();
		container.getAttachments().add(sr);
		container.getChildren().get(0).getAttachments().add(sr2);

		assertTrue(view.getChildren().get(0).isVisible());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
		container.getAttachments().remove(sr);
		assertTrue(container.isVisible());
		assertTrue(container.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEmptyEnableRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final EnableRule er = RuleFactory.eINSTANCE.createEnableRule();
		final EnableRule er2 = RuleFactory.eINSTANCE.createEnableRule();
		container.getAttachments().add(er);
		container.getChildren().get(0).getAttachments().add(er2);
		assertTrue(container.isEnabled());
		assertTrue(container.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddAndRemoveEmptyEnableRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final EnableRule er = RuleFactory.eINSTANCE.createEnableRule();
		final EnableRule er2 = RuleFactory.eINSTANCE.createEnableRule();
		container.getAttachments().add(er);
		container.getChildren().get(0).getAttachments().add(er2);
		assertTrue(container.isEnabled());
		assertTrue(container.getChildren().get(0).isEnabled());

		container.getAttachments().remove(er);
		assertTrue(container.isEnabled());
		assertTrue(container.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddShowRuleOnControl() {
		view = createSimpleView();
		initialize();

		final ShowRule sr = createShowRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(sr);
		assertFalse(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddAndRemoveShowRuleOnControl() {
		view = createSimpleView();
		initialize();

		final ShowRule sr = createShowRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(sr);
		assertFalse(view.getChildren().get(0).isVisible());
		view.getChildren().get(0).getAttachments().remove(sr);
		assertTrue(view.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEnableRuleOnControl() {
		view = createSimpleView();
		initialize();

		final EnableRule er = createEnableRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(er);
		assertFalse(view.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddAndRemoveEnableRuleOnControl() {
		view = createSimpleView();
		initialize();

		final EnableRule er = createEnableRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(er);
		assertFalse(view.getChildren().get(0).isEnabled());
		view.getChildren().get(0).getAttachments().remove(er);
		assertTrue(view.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddShowRuleOnContainer() {
		view = createContainerView();
		initialize();

		final ShowRule sr = createShowRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(sr);
		assertFalse(view.getChildren().get(0).isVisible());
		assertFalse(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
	}

	@Test
	public void testAddAndRemoveShowRuleOnContainer() {
		view = createContainerView();
		initialize();

		final ShowRule sr = createShowRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(sr);
		assertFalse(view.getChildren().get(0).isVisible());
		assertFalse(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
		view.getChildren().get(0).getAttachments().remove(sr);
		assertTrue(view.getChildren().get(0).isVisible());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEnableRuleOnContainer() {
		view = createContainerView();
		initialize();

		final EnableRule er = createEnableRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(er);
		assertFalse(view.getChildren().get(0).isEnabled());
		assertFalse(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddAndRemoveEnableRuleOnContainer() {
		view = createContainerView();
		initialize();

		final EnableRule er = createEnableRule(HEIGHT);
		view.getChildren().get(0).getAttachments().add(er);
		assertFalse(view.getChildren().get(0).isEnabled());
		assertFalse(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isEnabled());
		view.getChildren().get(0).getAttachments().remove(er);
		assertTrue(view.getChildren().get(0).isEnabled());
		assertTrue(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddShowRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final ShowRule sr = createShowRule(HEIGHT);
		final ShowRule sr2 = createShowRule(HEIGHT);
		container.getAttachments().add(sr);
		container.getChildren().get(0).getAttachments().add(sr2);

		assertFalse(container.isVisible());
		assertFalse(container.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddAndRemoveShowRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final ShowRule sr = createShowRule(HEIGHT);
		final ShowRule sr2 = createShowRule(HEIGHT);
		container.getAttachments().add(sr);
		container.getChildren().get(0).getAttachments().add(sr2);

		assertFalse(view.getChildren().get(0).isVisible());
		assertFalse(VContainedContainer.class.cast(view.getChildren().get(0)).getChildren().get(0).isVisible());
		container.getAttachments().remove(sr);
		assertTrue(container.isVisible());
		assertFalse(container.getChildren().get(0).isVisible());
	}

	@Test
	public void testAddEnableRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final EnableRule er = createEnableRule(HEIGHT);
		final EnableRule er2 = createEnableRule(HEIGHT);
		container.getAttachments().add(er);
		container.getChildren().get(0).getAttachments().add(er2);
		assertFalse(container.isEnabled());
		assertFalse(container.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddAndRemoveEnableRuleOnContainerAndControl() {
		view = createContainerView();
		initialize();

		final VContainedContainer container = VContainedContainer.class.cast(view.getChildren().get(0));
		final EnableRule er = createEnableRule(HEIGHT);
		final EnableRule er2 = createEnableRule(HEIGHT);
		container.getAttachments().add(er);
		container.getChildren().get(0).getAttachments().add(er2);
		assertFalse(container.isEnabled());
		assertFalse(container.getChildren().get(0).isEnabled());

		container.getAttachments().remove(er);
		assertTrue(container.isEnabled());
		assertFalse(container.getChildren().get(0).isEnabled());
	}

	@Test
	public void testAddNewVElementWithoutRule() {
		// setup
		view = createContainerView();
		initialize();

		// act
		final VVerticalLayout verticalLayout = getVerticalLayout(view);
		final VControl controlName = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference dmrName = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmrName.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		controlName.setDomainModelReference(dmrName);
		verticalLayout.getChildren().add(controlName);

		// assert
		// no exceptions expected
	}

	@Test
	public void testAddNewVElementWithLeafCondition() {
		// setup
		view = createContainerView();
		initialize();

		// act
		final VVerticalLayout verticalLayout = getVerticalLayout(view);
		final VControl controlName = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference dmrName = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmrName.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		controlName.setDomainModelReference(dmrName);

		final EnableRule er = createEnableRule(HEIGHT);
		controlName.getAttachments().add(er);

		verticalLayout.getChildren().add(controlName);

		// assert
		assertTrue(verticalLayout.getChildren().get(0).isEnabled());
		assertFalse(verticalLayout.getChildren().get(1).isEnabled());
	}

	@Test
	public void testAddNewVElementWithNonLeafCondition() {
		// setup
		view = createContainerView();
		initialize();

		// act
		final VVerticalLayout verticalLayout = getVerticalLayout(view);
		final VControl controlName = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference dmrName = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmrName.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		controlName.setDomainModelReference(dmrName);

		final EnableRule er = createEnableRule(HEIGHT, HEIGHT_ALT);
		controlName.getAttachments().add(er);

		verticalLayout.getChildren().add(controlName);

		// assert
		assertTrue(verticalLayout.getChildren().get(0).isEnabled());
		assertFalse(verticalLayout.getChildren().get(1).isEnabled());
	}

}
