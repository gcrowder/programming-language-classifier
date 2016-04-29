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
 * Edgar Mueller - additional test cases
 * Edgar Mueller - more test cases
 *******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.impl.BasicEObjectImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecp.common.spi.UniqueSetting;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.test.university.Assistant;
import org.eclipse.emf.ecp.test.university.Person;
import org.eclipse.emf.ecp.test.university.Professor;
import org.eclipse.emf.ecp.test.university.Staff;
import org.eclipse.emf.ecp.test.university.UniversityFactory;
import org.eclipse.emf.ecp.test.university.UniversityPackage;
import org.eclipse.emf.ecp.test.university.impl.StaffImpl;
import org.eclipse.emf.ecp.view.internal.rule.RuleService;
import org.eclipse.emf.ecp.view.internal.rule.RuleServiceHelperImpl;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.context.ViewModelService;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeListener;
import org.eclipse.emf.ecp.view.spi.model.VAttachment;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.RuleServiceHelper;
import org.eclipse.emf.ecp.view.spi.rule.model.AndCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.OrCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.Rule;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.ShowRule;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalFactory;
import org.eclipse.emf.ecp.view.spi.vertical.model.VVerticalLayout;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Merchandise;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.emf.emfstore.bowling.impl.FanImpl;
import org.eclipse.emf.emfstore.bowling.impl.LeagueImpl;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener;
import org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Things to be tested.
 *
 * tests on control with rule:
 * init wrong value , right value
 * lifecycle wrong -> right; right -> wrong; wrong -> wrong; right -> right
 * types: show, enable, hide, disable
 * (2+4)*4
 *
 * propagation tests:
 * - test propagation from parent to child (no rule) [combinations same as rule on control]
 * - test propagation from parent to child (rule != parent.rule) [combinations same as rule on control]
 *
 *
 * domain wrong
 * parent (s: disabled r:enable)
 * child1 (s:visible r:hidden) child2 (s:enabled r:disable)
 *
 * init
 * parent (s: disabled r:enable)
 * child1 (s:visible r:hidden) child2 (s:disabled r:disable)
 *
 * parent(s:enabled)
 * child (s:hidden) child2(s:disabled)
 *
 *
 * dispose
 *
 * getInvolvedObjects
 * objects if no change -> none, empty list
 * objects if change -> only container of rule
 * no side effects
 *
 * @author Eugen Neufeld
 * @author emueller
 */
public class RuleService_PTest extends CommonRuleTest {

	/**
	 * @author Jonas
	 *
	 */
	private class ViewModelContextStub implements ViewModelContext {

		private boolean hasRegisteredViewListener;
		private boolean hasRegisteredDomainListener;

		@Override
		public void unregisterViewChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredViewListener = false;
		}

		@Override
		public void unregisterDomainChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredDomainListener = false;
		}

		@Override
		public void registerViewChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredViewListener = true;
		}

		@Override
		public void registerDomainChangeListener(ModelChangeListener modelChangeListener) {
			hasRegisteredDomainListener = true;
		}

		@Override
		public VElement getViewModel() {
			return view;
		}

		@Override
		public EObject getDomainModel() {
			return null;
		}

		@Override
		public void dispose() {

		}

		@Override
		public <T> boolean hasService(Class<T> serviceType) {
			// not used in tests
			return false;
		}

		@Override
		public <T> T getService(Class<T> serviceType) {
			// not used in tests
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(org.eclipse.emf.ecore.EStructuralFeature.Setting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VControl> getControlsFor(Setting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(org.eclipse.emf.ecp.common.spi.UniqueSetting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VElement> getControlsFor(UniqueSetting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getContextValue(java.lang.String)
		 */
		@Override
		public Object getContextValue(String key) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#putContextValue(java.lang.String,
		 *      java.lang.Object)
		 */
		@Override
		public void putContextValue(String key, Object value) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getChildContext(org.eclipse.emf.ecore.EObject,
		 *      org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.model.VView,
		 *      org.eclipse.emf.ecp.view.spi.context.ViewModelService[])
		 */
		@Override
		public ViewModelContext getChildContext(EObject eObject, VElement parent, VView vView,
			ViewModelService... viewModelServices) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerDisposeListener(org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener)
		 */
		@Override
		public void registerDisposeListener(ViewModelContextDisposeListener listener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#addContextUser(java.lang.Object)
		 */
		@Override
		public void addContextUser(Object user) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#removeContextUser(java.lang.Object)
		 */
		@Override
		public void removeContextUser(Object user) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void registerEMFFormsContextListener(EMFFormsContextListener contextListener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void unregisterEMFFormsContextListener(EMFFormsContextListener contextListener) {
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getParentContext()
		 */
		@Override
		public ViewModelContext getParentContext() {
			// TODO Auto-generated method stub
			return null;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#changeDomainModel(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public void changeDomainModel(EObject newDomainModel) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerRootDomainModelChangeListener(org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener)
		 */
		@Override
		public void registerRootDomainModelChangeListener(RootDomainModelChangeListener rootDomainModelChangeListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterRootDomainModelChangeListener(org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener)
		 */
		@Override
		public void unregisterRootDomainModelChangeListener(
			RootDomainModelChangeListener rootDomainModelChangeListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getParentVElement()
		 */
		@Override
		public VElement getParentVElement() {
			// TODO Auto-generated method stub
			return null;
		}

	}

	/** The player. */
	private Player player;

	/** The league. */
	private League league;

	/** The view. */
	private VView view;

	/** The control p name. */
	private VControl controlPName;

	/** The column. */
	private VVerticalLayout column;

	/** The parent column. */
	private VVerticalLayout parentColumn;

	private ViewModelContext context;

	private DefaultRealm realm;

	/**
	 * Sets the up.
	 *
	 * @throws Exception the exception
	 */
	@Before
	public void setUp() throws Exception {
		realm = new DefaultRealm();
		player = BowlingFactory.eINSTANCE.createPlayer();
		league = BowlingFactory.eINSTANCE.createLeague();
		league.getPlayers().add(player);

		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(league.eClass());

		parentColumn = VVerticalFactory.eINSTANCE.createVerticalLayout();
		view.getChildren().add(parentColumn);

		column = VVerticalFactory.eINSTANCE.createVerticalLayout();
		parentColumn.getChildren().add(column);

		controlPName = VViewFactory.eINSTANCE.createControl();

		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setIndex(0);
		indexDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Players());
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		indexDMR.setTargetDMR(domainModelReference);
		controlPName.setDomainModelReference(indexDMR);

		column.getChildren().add(controlPName);

	}

	/**
	 * Tear down.
	 *
	 * @throws Exception the exception
	 */
	@After
	public void tearDown() throws Exception {
		if (context != null) {
			context.dispose();
		}
		realm.dispose();
	}

	private RuleService instantiateRuleService() {
		return instantiateRuleService(league);
	}

	/**
	 * Instantiate rule service.
	 *
	 * @return the rule service
	 */
	private RuleService instantiateRuleService(final EObject domainModel) {
		final RuleService ruleService = new RuleService();
		final RuleServiceHelperImpl ruleServiceHelper = new RuleServiceHelperImpl();
		context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, domainModel);
		ruleService.instantiate(context);
		ruleServiceHelper.instantiate(context);
		return ruleService;
	}

	/**
	 * Sets the league to wrong.
	 */
	private void setLeagueToWrong() {
		league.setName("League_Wrong");
	}

	/**
	 * Sets the league to right.
	 */
	private void setLeagueToRight() {
		league.setName("League");
	}

	private LeafCondition setupLeafConditionWithValueDMR(Object expectedValue, EReference domainRef,
		List<EReference> domainPath, EAttribute valueAtt, List<EReference> valuePath) {
		final LeafCondition leafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		leafCondition.setExpectedValue(expectedValue);

		final VFeaturePathDomainModelReference modelDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelDMR.getDomainModelEReferencePath().addAll(domainPath);
		modelDMR.setDomainModelEFeature(domainRef);
		leafCondition.setDomainModelReference(modelDMR);

		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR.getDomainModelEReferencePath().addAll(valuePath);
		valueDMR.setDomainModelEFeature(valueAtt);
		leafCondition.setValueDomainModelReference(valueDMR);

		return leafCondition;
	}

	private Setting getLeaguePlayersSetting() {
		return ((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Players());
	}

	@Test
	public void testInitialization() {
		final RuleService ruleService = new RuleService();
		final ViewModelContextStub contextStub = new ViewModelContextStub() {
			@Override
			public EObject getDomainModel() {
				return view;
			}
		};
		ruleService.instantiate(contextStub);
		ruleService.contextInitialised();
		assertTrue(contextStub.hasRegisteredViewListener);
		assertTrue(contextStub.hasRegisteredDomainListener);
	}

	@Test(expected = IllegalStateException.class)
	public void testInitializationWithNullDomainModel() {
		final RuleService ruleService = new RuleService();
		final ViewModelContextStub contextStub = new ViewModelContextStub();
		ruleService.instantiate(contextStub);
		ruleService.contextInitialised();
	}

	@Test(expected = IllegalStateException.class)
	public void testInitializationWithNullViewModel() {
		final RuleService ruleService = new RuleService();
		final ViewModelContextStub contextStub = new ViewModelContextStub();
		ruleService.instantiate(contextStub);
		ruleService.contextInitialised();
	}

	@Test
	public void testUnregisterOnViewModelContext() {
		final RuleService ruleService = new RuleService();
		final ViewModelContextStub contextStub = new ViewModelContextStub() {
			@Override
			public EObject getDomainModel() {
				return view;
			}
		};
		ruleService.instantiate(contextStub);
		ruleService.contextInitialised();
		assertTrue(contextStub.hasRegisteredViewListener);
		assertTrue(contextStub.hasRegisteredDomainListener);
		ruleService.dispose();
		assertFalse(contextStub.hasRegisteredViewListener);
		assertFalse(contextStub.hasRegisteredDomainListener);
	}

	/**
	 * Test init right show rule.
	 */
	@Test
	public void testInitRightShowRule() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test init wrong show rule.
	 */
	@Test
	public void testInitWrongShowRule() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule no child rule wrong init.
	 */
	@Test
	public void testPropagationShowRuleNoChildRuleWrongInit() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule no child rule right init.
	 */
	@Test
	public void testPropagationShowRuleNoChildRuleRightInit() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		setLeagueToRight();
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule child rule wrong init.
	 */
	@Test
	public void testPropagationShowRuleChildRuleWrongInit() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		addLeagueShowRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule child rule right init.
	 */
	@Test
	public void testPropagationShowRuleChildRuleRightInit() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		addLeagueShowRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with first condition being true.
	 * Controls should be visible.
	 */
	@Test
	public void testShowRuleWithOrConditionFirstConditionApplies() {
		addLeagueShowRuleWithOrCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(column.isVisible());
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with the second condition being
	 * true.
	 * Controls should be visible.
	 */
	@Test
	public void testShowRuleWithOrConditionSecondConditionApplies() {
		addLeagueShowRuleWithOrCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		instantiateRuleService();
		league.setName("League2");
		assertTrue(column.isVisible());
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with none of the conditions being
	 * true.
	 * Controls should not be visible.
	 */
	@Test
	public void testShowRuleWithOrConditionNoConditionApplies() {
		addLeagueShowRuleWithOrCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with first condition being true
	 * while
	 * initializing the rule service.
	 * Controls should be visible.
	 */
	@Test
	public void testInitShowRuleWithOrConditionFirstConditionApplies() {
		addLeagueShowRuleWithOrCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		setLeagueToRight();
		instantiateRuleService();
		assertTrue(column.isVisible());
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with second condition being true
	 * while
	 * initializing the rule service.
	 * Controls should be visible.
	 */
	@Test
	public void testInitShowRuleWithOrConditionSecondConditionApplies() {
		addLeagueShowRuleWithOrCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		league.setName("League2");
		instantiateRuleService();
		assertTrue(column.isVisible());
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with both conditions being true
	 * while
	 * initializing the rule service.
	 * Controls should be visible.
	 */
	@Test
	public void testInitShowRuleWithOrConditionBothConditionsApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);

		view.getChildren().add(control1);

		addLeagueShowRuleWithOrCondition(control1, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Name(), "foo"),
			createLeafCondition(BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3));
		player.setName("foo");
		player.setNumberOfVictories(3);
		instantiateRuleService(player);
		assertTrue(view.isVisible());
		assertTrue(control1.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with both conditions being true.
	 * Controls should be visible.
	 */
	@Test
	public void testShowRuleWithOrConditionBothConditionsApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);
		view.getChildren().add(control1);

		addLeagueShowRuleWithOrCondition(control1, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Name(), "foo"),
			createLeafCondition(BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3));
		instantiateRuleService(player);
		player.setName("foo");
		player.setNumberOfVictories(3);
		assertTrue(column.isVisible());
		assertTrue(control1.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.OrCondition OrCondition} with none of the conditions being
	 * true.
	 * Controls should not be visible.
	 */
	@Test
	public void testInitShowRuleWithOrConditionNoConditionApplies() {
		addLeagueShowRuleWithOrCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with first condition being true.
	 * Controls should not be visible.
	 */
	@Test
	public void testShowRuleWithAndConditionFirstConditionApplies() {
		addLeagueShowRuleWithAndCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with second condition being true.
	 * Controls should not be visible.
	 */
	@Test
	public void testShowRuleWithAndConditionSecondConditionApplies() {
		addLeagueShowRuleWithAndCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		instantiateRuleService();
		league.setName("League2");
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with none of the conditions being
	 * true.
	 * Controls should not be visible.
	 */
	@Test
	public void testShowRuleWithAndConditionNoConditionApplies() {
		addLeagueShowRuleWithAndCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with both conditions being true.
	 * Controls should be visible.
	 */
	@Test
	public void testShowRuleWithAndConditionBothConditionsApply() {

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Players());
		indexDMR.setIndex(0);
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		indexDMR.setTargetDMR(domainModelReference);
		control1.setDomainModelReference(indexDMR);
		column.getChildren().add(control1);

		addLeagueShowRuleWithAndCondition(
			column,
			true,
			setupLeafConditionWithValueDMR("foo",
				BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
				BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList()),
			setupLeafConditionWithValueDMR(3,
				BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
				BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), Collections.<EReference> emptyList()));
		instantiateRuleService(league);
		player.setName("foo");
		player.setNumberOfVictories(3);
		assertTrue(column.isVisible());
		assertTrue(control1.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with first condition being true
	 * while
	 * initializing the rule service.
	 * Controls should not be visible.
	 */
	@Test
	public void testInitShowRuleInitWithAndConditionFirstConditionApplies() {
		addLeagueShowRuleWithAndCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with second condition being true
	 * while
	 * initializing the rule service.
	 * Controls should not be visible.
	 */
	@Test
	public void testInitShowRuleWithAndConditionSecondConditionApplies() {
		addLeagueShowRuleWithAndCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		league.setName("League2");
		instantiateRuleService();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test {@link org.eclipse.emf.ecp.view.spi.rule.model.AndCondition AndCondition} with none of the conditions being
	 * true
	 * while initializing the rule service.
	 * Controls should not be visible.
	 */
	@Test
	public void testInitShowRuleWithAndConditionNoConditionApplies() {
		addLeagueShowRuleWithAndCondition(column, true,
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League"),
			createLeafCondition(BowlingPackage.eINSTANCE.getLeague_Name(), "League2"));
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(column.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testShowRuleWithTwoPossibleTargetsWhereOnlyOneSettingShouldApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);
		view.getChildren().add(control1);

		final VControl control2 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control2.setDomainModelReference(domainModelReference2);

		view.getChildren().add(control2);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getPlayer_Name(), "foo");
		addShowRule(control2, true, BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3);

		instantiateRuleService(player);
		player.setName("foo");
		assertTrue(control1.isVisible());
		assertFalse(control2.isVisible());
	}

	@Test
	public void testShowRuleWithTwoPossibleTargetsWhereTheOtherSettingShouldApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);

		view.getChildren().add(control1);

		final VControl control2 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control2.setDomainModelReference(domainModelReference2);

		view.getChildren().add(control2);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getPlayer_Name(), "foo");
		addShowRule(control2, true, BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3);

		instantiateRuleService(player);
		player.setNumberOfVictories(3);
		assertFalse(control1.isVisible());
		assertTrue(control2.isVisible());
	}

	@Test
	public void testShowRuleWhereConditionReferencesAnotherTarget() {
		final Fan fan = BowlingFactory.eINSTANCE.createFan();
		final Merchandise merchandise = BowlingFactory.eINSTANCE.createMerchandise();
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(fan.eClass());
		fan.setFavouriteMerchandise(merchandise);

		final VControl control1 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMerchandise_Name());
		domainModelReference.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());
		control1.setDomainModelReference(domainModelReference);

		view.getChildren().add(control1);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getMerchandise_Name(), "foo");

		Rule rule = null;
		for (final VAttachment attachment : control1.getAttachments()) {
			if (Rule.class.isInstance(attachment)) {
				rule = (Rule) attachment;
			}
		}

		((VFeaturePathDomainModelReference) ((LeafCondition) rule.getCondition()).getDomainModelReference())
			.getDomainModelEReferencePath().add(
				BowlingPackage.eINSTANCE.getFan_FavouriteMerchandise());

		instantiateRuleService(fan);
		merchandise.setName("foo");
		assertTrue(control1.isVisible());
	}

	@Test
	public void testShowRuleWithTwoPossibleTargetsWhereBothSettingShouldApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);

		view.getChildren().add(control1);

		final VControl control2 = VViewFactory.eINSTANCE.createControl();

		final VFeaturePathDomainModelReference domainModelReference2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control2.setDomainModelReference(domainModelReference2);

		view.getChildren().add(control2);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getPlayer_Name(), "foo");
		addShowRule(control2, true, BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3);

		instantiateRuleService(player);
		player.setName("foo");
		player.setNumberOfVictories(3);
		assertTrue(control1.isVisible());
		assertTrue(control2.isVisible());
	}

	@Test
	public void testInitShowRuleWithTwoPossibleTargetsWhereOnlyOneSettingShouldApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);
		view.getChildren().add(control1);

		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control2.setDomainModelReference(domainModelReference2);
		view.getChildren().add(control2);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getPlayer_Name(), "foo");
		addShowRule(control2, true, BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3);

		player.setName("foo");
		instantiateRuleService(player);
		assertTrue(control1.isVisible());
		assertFalse(control2.isVisible());
	}

	@Test
	public void testInitShowRuleWithTwoPossibleTargetsWhereTheOtherSettingShouldApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);
		view.getChildren().add(control1);

		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control2.setDomainModelReference(domainModelReference2);
		view.getChildren().add(control2);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getPlayer_Name(), "foo");
		addShowRule(control2, true, BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3);

		player.setNumberOfVictories(3);
		instantiateRuleService(player);
		assertFalse(control1.isVisible());
		assertTrue(control2.isVisible());
	}

	@Test
	public void testInitShowRuleWithTwoPossibleTargetsWhereBothSettingShouldApply() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(player.eClass());

		final VControl control1 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control1.setDomainModelReference(domainModelReference);
		view.getChildren().add(control1);

		final VControl control2 = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference domainModelReference2 = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Height());
		control2.setDomainModelReference(domainModelReference2);
		view.getChildren().add(control2);

		addShowRule(control1, true, BowlingPackage.eINSTANCE.getPlayer_Name(), "foo");
		addShowRule(control2, true, BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(), 3);

		player.setName("foo");
		player.setNumberOfVictories(3);
		instantiateRuleService(player);
		assertTrue(control1.isVisible());
		assertTrue(control2.isVisible());
	}

	/**
	 * Test default wrong to right show rule.
	 */
	@Test
	public void testDefaultWrongToRightShowRule() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test default wrong to wrong show rule.
	 */
	@Test
	public void testDefaultWrongToWrongShowRule() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test default right to wrong show rule.
	 */
	@Test
	public void testDefaultRightToWrongShowRule() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test default right to right show rule.
	 */
	@Test
	public void testDefaultRightToRightShowRule() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule no child rule wrong to right.
	 */
	@Test
	public void testPropagationShowRuleNoChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule no child rule wrong to wrong.
	 */
	@Test
	public void testPropagationShowRuleNoChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule no child rule right to wrong.
	 */
	@Test
	public void testPropagationShowRuleNoChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule no child rule right to right.
	 */
	@Test
	public void testPropagationShowRuleNoChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule child rule wrong to right.
	 */
	@Test
	public void testPropagationShowRuleChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		addLeagueShowRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule child rule wrong to wrong.
	 */
	@Test
	public void testPropagationShowRuleChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		addLeagueShowRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule child rule right to wrong.
	 *
	 * Control should not be visible since its parent will be hidden.
	 */
	@Test
	public void testPropagationShowRuleChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		addLeagueShowRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation show rule child rule right to right.
	 */
	@Test
	public void testPropagationShowRuleChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be visible

		addLeagueShowRule(column, true);
		addLeagueShowRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test init right enable rule.
	 */
	@Test
	public void testInitRightEnableRule() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test init wrong enable rule.
	 */
	@Test
	public void testInitWrongEnableRule() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule no child rule wrong init.
	 */
	@Test
	public void testPropagationEnableRuleNoChildRuleWrongInit() {
		addLeagueEnableRule(column, true);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule no child rule right init.
	 */
	@Test
	public void testPropagationEnableRuleNoChildRuleRightInit() {
		addLeagueEnableRule(column, true);
		setLeagueToRight();
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation enable rule child rule wrong init.
	 */
	@Test
	public void testPropagationEnableRuleChildRuleWrongInit() {
		addLeagueEnableRule(column, true);
		addLeagueEnableRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule child rule right init.
	 */
	@Test
	public void testPropagationEnableRuleChildRuleRightInit() {
		addLeagueEnableRule(column, true);
		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test default wrong to right enable rule.
	 */
	@Test
	public void testDefaultWrongToRightEnableRule() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test default wrong to wrong enable rule.
	 */
	@Test
	public void testDefaultWrongToWrongEnableRule() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test default right to wrong enable rule.
	 */
	@Test
	public void testDefaultRightToWrongEnableRule() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test default right to right enable rule.
	 */
	@Test
	public void testDefaultRightToRightEnableRule() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule no child rule wrong to right.
	 */
	@Test
	public void testPropagationEnableRuleNoChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule no child rule wrong to wrong.
	 */
	@Test
	public void testPropagationEnableRuleNoChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule no child rule right to wrong.
	 */
	@Test
	public void testPropagationEnableRuleNoChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule no child rule right to right.
	 */
	@Test
	public void testPropagationEnableRuleNoChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule child rule wrong to right.
	 */
	@Test
	public void testPropagationEnableRuleChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		addLeagueEnableRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule child rule wrong to wrong.
	 */
	@Test
	public void testPropagationEnableRuleChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		addLeagueEnableRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule child rule right to wrong.
	 */
	@Test
	public void testPropagationEnableRuleChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation enable rule child rule right to right.
	 */
	@Test
	public void testPropagationEnableRuleChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(column, true);
		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test init right hide rule.
	 */
	@Test
	public void testInitRightHideRule() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test init wrong hide rule.
	 */
	@Test
	public void testInitWrongHideRule() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule no child rule right init.
	 */
	@Test
	public void testPropagationHideRuleNoChildRuleRightInit() {
		addLeagueShowRule(column, false);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule no child rule wrong init.
	 */
	@Test
	public void testPropagationHideRuleNoChildRuleWrongInit() {
		addLeagueShowRule(column, false);
		setLeagueToWrong();
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule child rule right init.
	 */
	@Test
	public void testPropagationHideRuleChildRuleRightInit() {
		addLeagueShowRule(column, false);
		addLeagueShowRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule child rule wrong init.
	 */
	@Test
	public void testPropagationHideRuleChildRuleWrongInit() {
		addLeagueShowRule(column, false);
		addLeagueShowRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test default wrong to right hide rule.
	 */
	@Test
	public void testDefaultWrongToRightHideRule() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test default wrong to wrong hide rule.
	 */
	@Test
	public void testDefaultWrongToWrongHideRule() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test default right to wrong hide rule.
	 */
	@Test
	public void testDefaultRightToWrongHideRule() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test default right to right hide rule.
	 */
	@Test
	public void testDefaultRightToRightHideRule() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule no child rule wrong to right.
	 */
	@Test
	public void testPropagationHideRuleNoChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule no child rule wrong to wrong.
	 */
	@Test
	public void testPropagationHideRuleNoChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isVisible());
	}

	@Test
	public void testContainmentConditionWithShowRule() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("asdf@asdf.com",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_EMails(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();

		assertFalse(controlPName.isVisible());

		player.getEMails().add("asdf@asdf.com");

		assertTrue(controlPName.isVisible());
	}

	@Test
	public void testGetInvolvedEObjetsContainedMultiAttributeNoMatch() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("asdf@asdf.com",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_EMails(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();

		player.getEMails().add("foo@bar.com");
		final Player newPlayer = BowlingFactory.eINSTANCE.createPlayer();
		newPlayer.getEMails().add("asdf@asdf.com");

		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final List<Player> newValue = new ArrayList<Player>();
		newValue.add(EcoreUtil.copy(player));
		newValue.get(0).getEMails().clear();
		newValue.add(newPlayer);

		final Set<VControl> involvedEObjects = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Players()), newValue, VControl.class);

		assertEquals(0, involvedEObjects.size());
	}

	@Test
	public void testGetInvolvedEObjetsContainedMultiAttributeEmptySet() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("asdf@asdf.com",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_EMails(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();

		player.getEMails().add("foo@bar.com");
		final Player newPlayer = BowlingFactory.eINSTANCE.createPlayer();
		newPlayer.getEMails().add("asdf@asdf.com");

		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedEObjects = helper.getInvolvedEObjects(getLeaguePlayersSetting(),
			new ArrayList<Player>(), VControl.class);

		assertEquals(0, involvedEObjects.size());
	}

	@Test
	public void testGetInvolvedEObjetsContainedMultiAttributeNoMatchViaUpdatePlayer() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("asdf@asdf.com",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_EMails(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();

		player.getEMails().add("foo@bar.com");

		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final List<Player> newValues = new ArrayList<Player>();
		newValues.add(EcoreUtil.copy(player));
		newValues.get(0).getEMails().add("asdf@asdf.com");

		final Set<VControl> involvedEObjects = helper.getInvolvedEObjects(getLeaguePlayersSetting(), newValues,
			VControl.class);

		assertEquals(0, involvedEObjects.size());
	}

	@Test
	public void testGetInvolvedEObjetsContainedMultiAttributeMatch() {
		final ShowRule showRule = addShowRule(controlPName, true);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("asdf@asdf.com",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_EMails(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();

		player.getEMails().add("foo@bar.com");
		final Player newPlayer = BowlingFactory.eINSTANCE.createPlayer();
		newPlayer.getEMails().add("asdf@asdf.com");

		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);
		final Map<Setting, Object> expectedValues = new LinkedHashMap<EStructuralFeature.Setting, Object>();

		expectedValues.put(
			((BasicEObjectImpl) player).eSetting(BowlingPackage.eINSTANCE.getPlayer_EMails()),
			player.getEMails());
		expectedValues.put(
			((BasicEObjectImpl) newPlayer).eSetting(BowlingPackage.eINSTANCE.getPlayer_EMails()),
			newPlayer.getEMails());

		final List<Player> newValue = new ArrayList<Player>();
		newValue.add(player);
		newValue.add(newPlayer);

		final Set<VControl> involvedEObjects = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Players()),
			newValue, VControl.class);

		assertEquals(1, involvedEObjects.size());
	}

	@Test
	public void testGetInvolvedEObjetsContainedMultiAttributeMatchViaNewEmptyListValue() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("asdf@asdf.com",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_EMails(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();

		player.getEMails().add("asdf@asdf.com");

		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final List<Player> newValue = new ArrayList<Player>();
		newValue.add(EcoreUtil.copy(player));
		newValue.get(0).getEMails().clear();

		final Set<VControl> involvedEObjects = helper.getInvolvedEObjects(getLeaguePlayersSetting(), newValue,
			VControl.class);

		assertEquals(1, involvedEObjects.size());
	}

	@Test
	public void testContainmentConditionWithShowRuleWithClearedMultiref() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("doe",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		player.setName("doe");
		instantiateRuleService();
		assertTrue(controlPName.isVisible());
		league.getPlayers().clear();
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testContainmentConditionWithShowRuleFulFillCondition() {
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("doe",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
		player.setName("doe");
		assertTrue(controlPName.isVisible());
	}

	@Test
	public void testContainmentConditionInitEmptyList() {
		league = BowlingFactory.eINSTANCE.createLeague();
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("doe",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testContainmentConditionAddToEmptyList() {
		league = BowlingFactory.eINSTANCE.createLeague();
		final ShowRule showRule = addShowRule(controlPName, false);
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR("doe",
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList());
		showRule.setCondition(leafCondition1);
		instantiateRuleService();
		assertFalse(controlPName.isVisible());
		league.getPlayers().add(player);
		player.setName("doe");
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule no child rule right to wrong.
	 */
	@Test
	public void testPropagationHideRuleNoChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule no child rule right to right.
	 */
	@Test
	public void testPropagationHideRuleNoChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule child rule wrong to right.
	 */
	@Test
	public void testPropagationHideRuleChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		addLeagueShowRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule child rule wrong to wrong.
	 */
	@Test
	public void testPropagationHideRuleChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		addLeagueShowRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule child rule right to wrong.
	 */
	@Test
	public void testPropagationHideRuleChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		addLeagueShowRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test propagation hide rule child rule right to right.
	 */
	@Test
	public void testPropagationHideRuleChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be invisible

		addLeagueShowRule(column, false);
		addLeagueShowRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isVisible());
	}

	/**
	 * Test init right disable rule.
	 */
	@Test
	public void testInitRightDisableRule() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test init wrong disable rule.
	 */
	@Test
	public void testInitWrongDisableRule() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule no child rule right init.
	 */
	@Test
	public void testPropagationDisableRuleNoChildRuleRightInit() {
		addLeagueEnableRule(column, false);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule no child rule wrong init.
	 */
	@Test
	public void testPropagationDisableRuleNoChildRuleWrongInit() {
		addLeagueEnableRule(column, false);
		setLeagueToWrong();
		instantiateRuleService();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule child rule right init.
	 */
	@Test
	public void testPropagationDisableRuleChildRuleRightInit() {
		addLeagueEnableRule(column, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule child rule wrong init.
	 */
	@Test
	public void testPropagationDisableRuleChildRuleWrongInit() {
		addLeagueEnableRule(column, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test default right to wrong disable rule.
	 */
	@Test
	public void testDefaultRightToWrongDisableRule() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test default right to right disable rule.
	 */
	@Test
	public void testDefaultRightToRightDisableRule() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test default wrong to right disable rule.
	 */
	@Test
	public void testDefaultWrongToRightDisableRule() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test default wrong to wrong disable rule.
	 */
	@Test
	public void testDefaultWrongToWrongDisableRule() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(controlPName, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule no child rule right to wrong.
	 */
	@Test
	public void testPropagationDisableRuleNoChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule no child rule right to right.
	 */
	@Test
	public void testPropagationDisableRuleNoChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule no child rule wrong to right.
	 */
	@Test
	public void testPropagationDisableRuleNoChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule no child rule wrong to wrong.
	 */
	@Test
	public void testPropagationDisableRuleNoChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule child rule right to wrong.
	 */
	@Test
	public void testPropagationDisableRuleChildRuleRightToWrong() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule child rule right to right.
	 */
	@Test
	public void testPropagationDisableRuleChildRuleRightToRight() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule child rule wrong to right.
	 */
	@Test
	public void testPropagationDisableRuleChildRuleWrongToRight() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToRight();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test propagation disable rule child rule wrong to wrong.
	 */
	@Test
	public void testPropagationDisableRuleChildRuleWrongToWrong() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(column, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToWrong();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test multi level propagation on init.
	 */
	@Test
	public void testMultiLevelPropagationOnInit() {
		addLeagueEnableRule(parentColumn, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test multi level propagation on lifecycle.
	 */
	@Test
	public void testMultiLevelPropagationOnLifecycle() {
		// if the expected value equals the model value, then the control should be disabled

		addLeagueEnableRule(parentColumn, false);
		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		setLeagueToWrong();
		assertFalse(controlPName.isEnabled());
	}

	@Test
	public void testParentShowVsChildShow() {
		addShowRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertTrue(parentColumn.isVisible());
		assertTrue(controlPName.isVisible());
	}

	@Test
	public void testParentShowVsChildHide() {
		addShowRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertTrue(parentColumn.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testParentHideVsChildHide() {
		addShowRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertFalse(parentColumn.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testParentHideVsChildShow() {
		addShowRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertFalse(parentColumn.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testParentEnableVsChildEnable() {
		addEnableRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertTrue(parentColumn.isEnabled());
		assertTrue(controlPName.isEnabled());
	}

	@Test
	public void testParentEnableVsChildDisable() {
		addEnableRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertTrue(parentColumn.isEnabled());
		assertFalse(controlPName.isEnabled());
	}

	@Test
	public void testParentDisableVsChildDisable() {
		addEnableRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertFalse(parentColumn.isEnabled());
		assertFalse(controlPName.isEnabled());
	}

	@Test
	public void testParentDisableVsChildEnable() {
		addEnableRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		instantiateRuleService();
		league.setName("foo");
		assertFalse(parentColumn.isEnabled());
		assertFalse(controlPName.isEnabled());
	}

	@Test
	public void testInitParentShowVsChildShow() {
		addShowRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertTrue(parentColumn.isVisible());
		assertTrue(controlPName.isVisible());
	}

	@Test
	public void testInitParentShowVsChildHide() {
		addShowRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertTrue(parentColumn.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testInitParentHideVsChildHide() {
		addShowRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertFalse(parentColumn.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testInitParentHideVsChildShow() {
		addShowRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addShowRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertFalse(parentColumn.isVisible());
		assertFalse(controlPName.isVisible());
	}

	@Test
	public void testInitParentEnableVsChildEnable() {
		addEnableRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertTrue(parentColumn.isEnabled());
		assertTrue(controlPName.isEnabled());
	}

	@Test
	public void testInitParentEnableVsChildDisable() {
		addEnableRule(parentColumn, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertTrue(parentColumn.isEnabled());
		assertFalse(controlPName.isEnabled());
	}

	@Test
	public void testInitParentDisableVsChildDisable() {
		addEnableRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertFalse(parentColumn.isEnabled());
		assertFalse(controlPName.isEnabled());
	}

	@Test
	public void testInitParentDisableVsChildEnable() {
		addEnableRule(parentColumn, false, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		addEnableRule(controlPName, true, BowlingPackage.eINSTANCE.getLeague_Name(), "foo");
		league.setName("foo");
		instantiateRuleService();
		assertFalse(parentColumn.isEnabled());
		assertFalse(controlPName.isEnabled());
	}

	/**
	 * Test dispose.
	 */
	@Test
	public void testDispose() {
		addLeagueEnableRule(controlPName, false);
		setLeagueToRight();
		final RuleService ruleService = instantiateRuleService();
		ruleService.dispose();
		setLeagueToWrong();
		assertTrue(controlPName.isEnabled());
	}

	/**
	 * Test get involved e object no change.
	 */
	@Test
	public void testGetInvolvedEObjectNoChange() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		final RuleService ruleService = instantiateRuleService();

		final Map<VElement, Boolean> involvedEObjects = ruleService.getDisabledRenderables(
			createSettingsMapping(((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()), "League"),
			UniqueSetting.createSetting(league, BowlingPackage.eINSTANCE.getLeague_Name()));
		assertTrue(involvedEObjects.isEmpty());
	}

	/**
	 * Test get involved e object no side effects.
	 */
	@Test
	public void testGetInvolvedEObjectNoSideEffects() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		final RuleService ruleService = instantiateRuleService();

		ruleService.getDisabledRenderables(
			createSettingsMapping(((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()), "League"),
			UniqueSetting.createSetting(league, BowlingPackage.eINSTANCE.getLeague_Name()));
		assertTrue(controlPName.isEnabled());
	}

	private static Map<Setting, Object> createSettingsMapping(Setting setting, Object newValue) {
		final Map<Setting, Object> result = new LinkedHashMap<EStructuralFeature.Setting, Object>();
		result.put(setting, newValue);
		return result;
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectChange() {

		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToRight();
		final RuleService ruleService = instantiateRuleService();
		ruleService.contextInitialised();

		final Map<VElement, Boolean> disabledRenderables = ruleService.getDisabledRenderables(
			createSettingsMapping(((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
				"League_Wrong"),
			UniqueSetting.createSetting(league, BowlingPackage.eINSTANCE.getLeague_Name()));
		final Map<VElement, Boolean> hiddenRenderables = ruleService.getHiddenRenderables(
			createSettingsMapping(((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
				"League_Wrong"),
			UniqueSetting.createSetting(league, BowlingPackage.eINSTANCE.getLeague_Name()));

		assertEquals(3, hiddenRenderables.size());
		assertTrue(hiddenRenderables.containsKey(parentColumn));
		assertTrue(hiddenRenderables.containsKey(column));
		assertTrue(hiddenRenderables.containsKey(controlPName));

		assertEquals(1, disabledRenderables.size());
		assertTrue(disabledRenderables.containsKey(controlPName));
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectChangeWithoutChangedValue() {
		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToRight();
		final RuleService ruleService = instantiateRuleService();

		final Map<VElement, Boolean> disabledRenderables = ruleService.getDisabledRenderables(
			createSettingsMapping(((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
				"League"),
			UniqueSetting.createSetting(league, BowlingPackage.eINSTANCE.getLeague_Name()));
		final Map<VElement, Boolean> hiddenRenderables = ruleService.getHiddenRenderables(
			createSettingsMapping(((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
				"League"),
			UniqueSetting.createSetting(league, BowlingPackage.eINSTANCE.getLeague_Name()));

		assertEquals(0, disabledRenderables.size());
		assertEquals(0, hiddenRenderables.size());
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectNoVanishingRenderables() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToRight();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedEControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League", VControl.class);
		assertEquals(0, involvedEControls.size());
	}

	/**
	 * Should return the control. Changing the league's name to
	 * 'League2' should not alter the disabled/show state of the
	 * control.
	 */
	@Test
	public void testGetInvolvedEObjectsHelperBothRulesApply() {
		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToWrong();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedEControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VControl.class);
		assertEquals(0, involvedEControls.size());
	}

	/**
	 * Should return the parent columns of the control.
	 * Changing the league's name to 'League2' should not alter
	 * the disabled/show state of the control.
	 */
	@Test
	public void testGetInvolvedEObjectsHelperNoRuleAppliesFilterForComposite() {
		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToWrong();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VVerticalLayout> involvedEControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VVerticalLayout.class);
		assertEquals(0, involvedEControls.size());
	}

	/**
	 * Should return nothing since the correct value is set.
	 */
	@Test
	public void testGetInvolvedEObjectsHelperNoRuleApplies() {
		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToWrong();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedEControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League", VControl.class);
		assertEquals(0, involvedEControls.size());
	}

	/**
	 * Should return the control since the disable rule applies because
	 * of the wrong value.
	 */
	@Test
	public void testGetInvolvedEObjectsHelperEnableRuleAppliesWrongValue() {
		addLeagueEnableRule(controlPName, false);
		addLeagueShowRule(parentColumn, true);
		setLeagueToWrong();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedEControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VControl.class);
		assertEquals(0, involvedEControls.size());
	}

	/**
	 * Should return the control because of the {@link org.eclipse.emf.ecp.view.spi.rule.model.EnableRule EnableRule} on
	 * the
	 * control.
	 */
	@Test
	public void testGetInvolvedEObjectsHelperEnableRuleAppliesCorrectValue() {
		addLeagueEnableRule(controlPName, false);
		addLeagueShowRule(parentColumn, true);
		setLeagueToWrong();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> hiddenOrDisabledControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League", VControl.class);

		assertEquals(1, hiddenOrDisabledControls.size());
	}

	/**
	 * Should return nothing because the column should be visible, since
	 * 'League2' is the wrong value and the we have a disable rule.
	 */

	/**
	 * Should return the control because the column should be hidden, since
	 * 'League' is the right value and the we have a disable rule.
	 */
	@Test
	public void testGetInvolvedEObjectsHelperShowRuleAppliesCorrectValue() {
		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, false);
		setLeagueToWrong();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedEControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League", VControl.class);
		assertEquals(1, involvedEControls.size());
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectParentAffectedIfChildChanged() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VVerticalLayout> involvedColumns = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VVerticalLayout.class);
		assertEquals(1, involvedColumns.size());
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectParentAffectedIfChildNotChanged() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		setLeagueToRight();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VVerticalLayout> involvedColumns = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League", VVerticalLayout.class);
		assertEquals(0, involvedColumns.size());
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectWithVanishingRenderableShowRuleApplies() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueShowRule(parentColumn, true);
		setLeagueToRight();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VControl.class);
		assertEquals(1, involvedControls.size());
		assertTrue(involvedControls.contains(controlPName));
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectWithVanishingRenderableEnableRuleApplies() {
		addLeagueEnableRule(controlPName, true);

		setLeagueToRight();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VControl.class);
		assertEquals(1, involvedControls.size());
		assertTrue(involvedControls.contains(controlPName));
	}

	/**
	 * Test get involved e object change.
	 */
	@Test
	public void testGetInvolvedEObjectWithVanishingRenderableTwoRulesApply() {
		// if the expected value equals the model value, then the control should be enabled

		addLeagueEnableRule(controlPName, true);
		addLeagueShowRule(parentColumn, true);
		setLeagueToRight();
		instantiateRuleService();
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final Set<VControl> involvedControls = helper.getInvolvedEObjects(
			((LeagueImpl) league).eSetting(BowlingPackage.eINSTANCE.getLeague_Name()),
			"League2", VControl.class);
		assertEquals(1, involvedControls.size());
		assertTrue(involvedControls.contains(controlPName));
	}

	@Test
	public void testGetInvolvedWithUnrelatedChangeInMultiReference() {
		// domain
		final Staff staff = UniversityFactory.eINSTANCE.createStaff();
		final Assistant assistant = UniversityFactory.eINSTANCE.createAssistant();
		assistant.setName("Josef");
		final Professor professor = UniversityFactory.eINSTANCE.createProfessor();
		staff.getAssistants().add(assistant);
		staff.getProfessors().add(professor);

		// view
		view = VViewFactory.eINSTANCE.createView();
		final VControl wholeStaff = VViewFactory.eINSTANCE.createControl();
		wholeStaff.setDomainModelReference(UniversityPackage.eINSTANCE.getStaff_Staff());
		view.getChildren().add(wholeStaff);

		// rules
		final LeafCondition condition = setupLeafConditionWithValueDMR("Josef",
			UniversityPackage.eINSTANCE.getStaff_Assistants(), Collections.<EReference> emptyList(),
			UniversityPackage.eINSTANCE.getPerson_Name(), Collections.<EReference> emptyList());
		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		rule.setHide(false);
		rule.setCondition(condition);
		wholeStaff.getAttachments().add(rule);

		// init
		instantiateRuleService(staff);
		assertTrue(wholeStaff.isVisible());

		// act
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);

		final List<Person> newValue = new ArrayList<Person>();
		newValue.add(EcoreUtil.copy(professor));
		newValue.get(0).setName("Hans");

		final Setting setting = ((StaffImpl) staff).eSetting(UniversityPackage.eINSTANCE.getStaff_Professors());

		final Set<VControl> involvedControls = helper.getInvolvedEObjects(setting, newValue, VControl.class);

		// assert
		assertTrue(wholeStaff.isVisible());
		assertTrue(involvedControls.isEmpty());

		professor.setName("Hans");
		assertTrue(wholeStaff.isVisible());

	}

	@Test
	public void testGetInvolvedWithAndOrConditions() {
		// setup
		final Fan fan = BowlingFactory.eINSTANCE.createFan();
		fan.setHasSeasonTicket(true);
		fan.setNumberOfTournamentsVisited(2);

		final ShowRule rule = RuleFactory.eINSTANCE.createShowRule();
		final AndCondition andCondition = RuleFactory.eINSTANCE.createAndCondition();
		final LeafCondition seasonTicketCondition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference seasonTicketDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		seasonTicketDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_HasSeasonTicket());
		seasonTicketCondition.setDomainModelReference(seasonTicketDMR);
		seasonTicketCondition.setExpectedValue(true);
		andCondition.getConditions().add(seasonTicketCondition);
		final OrCondition orCondition = RuleFactory.eINSTANCE.createOrCondition();
		final LeafCondition visitedTwoLeafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference visitedTwoDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		visitedTwoDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_NumberOfTournamentsVisited());
		visitedTwoLeafCondition.setDomainModelReference(visitedTwoDMR);
		visitedTwoLeafCondition.setExpectedValue(2);
		orCondition.getConditions().add(visitedTwoLeafCondition);
		final LeafCondition visitedThreeLeafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference visitedThreeDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		visitedThreeDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_NumberOfTournamentsVisited());
		visitedThreeLeafCondition.setDomainModelReference(visitedThreeDMR);
		visitedThreeLeafCondition.setExpectedValue(3);
		orCondition.getConditions().add(visitedThreeLeafCondition);
		andCondition.getConditions().add(orCondition);
		rule.setCondition(andCondition);

		view = VViewFactory.eINSTANCE.createView();
		final VControl control = VViewFactory.eINSTANCE.createControl();
		control.setDomainModelReference(BowlingPackage.eINSTANCE.getFan_Gender());
		control.getAttachments().add(rule);
		view.getChildren().add(control);

		// init
		instantiateRuleService(fan);
		assertTrue(control.isVisible());

		// act
		final RuleServiceHelper helper = context.getService(RuleServiceHelper.class);
		final Setting setting = ((FanImpl) fan).eSetting(BowlingPackage.eINSTANCE.getFan_NumberOfTournamentsVisited());
		final Set<VControl> involvedControls = helper.getInvolvedEObjects(setting, 3, VControl.class);

		// assert
		assertTrue(involvedControls.isEmpty());
	}
}
