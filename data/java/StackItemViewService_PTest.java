/**
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 */
package org.eclipse.emf.ecp.view.stack.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.internal.stack.ui.swt.StackItemViewService;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackItem;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackLayout;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class StackItemViewService_PTest {

	private static final String NAME = "Name";
	private DefaultRealm realm;
	private Player domain;
	private ViewModelContext context;
	private VView view;
	private VStackLayout stackLayout;
	private VStackItem femaleItem;
	private VStackItem maleItem;

	@After
	public void after() {
		context.dispose();
		realm.dispose();
	}

	@Before
	public void before() {
		realm = new DefaultRealm();

		domain = BowlingFactory.eINSTANCE.createPlayer();

		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getPlayer());

		stackLayout = TestUtil.createStack(BowlingPackage.eINSTANCE.getPlayer_Gender());
		view.getChildren().add(stackLayout);

		femaleItem = TestUtil.createItem(Gender.FEMALE, BowlingPackage.eINSTANCE.getPlayer_DateOfBirth());
		stackLayout.getStackItems().add(femaleItem);

		maleItem = TestUtil.createItem(Gender.MALE, BowlingPackage.eINSTANCE.getPlayer_EMails());
		stackLayout.getStackItems().add(maleItem);
	}

	private void domainToMale() {
		domain.setGender(Gender.MALE);
	}

	private void domainToFemale() {
		domain.setGender(Gender.FEMALE);
	}

	private StackItemViewService viewService() {
		final StackItemViewService stackService = new StackItemViewService();
		context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
		stackService.instantiate(context);
		return stackService;
	}

	@Test
	public void initMale() {
		domainToMale();
		viewService();
		assertEquals(maleItem, stackLayout.getTopElement());
	}

	@Test
	public void initFemale() {
		domainToFemale();
		viewService();
		assertEquals(femaleItem, stackLayout.getTopElement());
	}

	@Test
	public void initMultipleStacksDifferentDMRs() {
		final VStackLayout stack2 = TestUtil.createStack(BowlingPackage.eINSTANCE.getPlayer_Name());
		final VStackItem item2 = TestUtil.createItem(NAME, BowlingPackage.eINSTANCE.getPlayer_IsProfessional());
		stack2.getStackItems().add(item2);
		view.getChildren().add(stack2);
		domainToMale();
		domain.setName(NAME);

		viewService();

		assertEquals(maleItem, stackLayout.getTopElement());
		assertEquals(item2, stack2.getTopElement());
	}

	@Test
	public void initMultipleStacksSameDMRs() {
		final VStackLayout stack2 = TestUtil.createStack(BowlingPackage.eINSTANCE.getPlayer_Gender());
		final VStackItem item2 = TestUtil
			.createItem(Gender.FEMALE, BowlingPackage.eINSTANCE.getPlayer_IsProfessional());
		stack2.getStackItems().add(item2);
		view.getChildren().add(stack2);
		domainToFemale();

		viewService();

		assertEquals(femaleItem, stackLayout.getTopElement());
		assertEquals(item2, stack2.getTopElement());
	}

	@Test
	public void initWithNullExpected() {
		stackLayout.setDomainModelReference(TestUtil.createDMR(BowlingPackage.eINSTANCE.getPlayer_Name()));
		maleItem.setValue(null);
		domain.setName(null);
		viewService();
		assertEquals(maleItem, stackLayout.getTopElement());
	}

	/**
	 * Should not cause exceptions.
	 */
	@Test
	public void initWithoutDMR() {
		stackLayout.setDomainModelReference(null);
		viewService();
		assertNull(stackLayout.getTopElement());
	}

	/**
	 * Should not cause exceptions.
	 */
	@Test
	public void initWithUnusableDMR() {
		stackLayout.setDomainModelReference(TestUtil.createDMR(BowlingPackage.eINSTANCE.getFan_DateOfBirth()));
		viewService();
		assertNull(stackLayout.getTopElement());
	}

	@Test
	public void dynamicFemaleToMale() {
		domainToFemale();
		viewService();
		domainToMale();
		assertEquals(maleItem, stackLayout.getTopElement());
	}

	/**
	 * Should not cause problems.
	 */
	@Test
	public void dynamicUnrelatedChange() {
		domainToFemale();
		viewService();
		domain.setName(NAME);
		assertEquals(femaleItem, stackLayout.getTopElement());
	}

	@Test
	public void enumSetToValueWithoutItem() {
		stackLayout.getStackItems().remove(maleItem);
		domainToFemale();
		viewService();
		assertEquals(femaleItem, stackLayout.getTopElement());
		domainToMale();
		assertNull(stackLayout.getTopElement());
	}

}
