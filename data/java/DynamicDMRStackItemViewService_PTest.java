/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.stack.ui.swt.test;

import static org.junit.Assert.assertEquals;

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
import org.eclipse.emf.emfstore.bowling.Fan;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class DynamicDMRStackItemViewService_PTest {

	private DefaultRealm realm;
	private Fan fan;
	private ViewModelContext context;
	private VView view;
	private VStackLayout stackLayout;
	private VStackItem femaleItem;
	private VStackItem maleItem;
	private Player player1;
	private Player player2;

	@After
	public void after() {
		if (context != null) {
			context.dispose();
		}
		realm.dispose();
	}

	@Before
	public void before() {
		realm = new DefaultRealm();

		fan = BowlingFactory.eINSTANCE.createFan();
		player1 = BowlingFactory.eINSTANCE.createPlayer();
		player2 = BowlingFactory.eINSTANCE.createPlayer();

		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());

		stackLayout = TestUtil.createStack(BowlingPackage.eINSTANCE.getPlayer_Gender(),
			BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		view.getChildren().add(stackLayout);

		femaleItem = TestUtil.createItem(Gender.FEMALE, BowlingPackage.eINSTANCE.getPlayer_DateOfBirth());
		stackLayout.getStackItems().add(femaleItem);

		maleItem = TestUtil.createItem(Gender.MALE, BowlingPackage.eINSTANCE.getPlayer_EMails());
		stackLayout.getStackItems().add(maleItem);
	}

	private void toMale(Player player) {
		player.setGender(Gender.MALE);
	}

	private void toFemale(Player player) {
		player.setGender(Gender.FEMALE);
	}

	private void favouritePlayer(Player player) {
		fan.setFavouritePlayer(player);
	}

	private StackItemViewService viewService() {
		final StackItemViewService unsetService = new StackItemViewService();
		context = ViewModelContextFactory.INSTANCE.createViewModelContext(view, fan);
		unsetService.instantiate(context);
		return unsetService;
	}

	@Test
	public void changeFavouritePlayer() {
		toMale(player1);
		toFemale(player2);
		favouritePlayer(player1);
		viewService();
		assertEquals(maleItem, stackLayout.getTopElement());
		favouritePlayer(player2);
		assertEquals(femaleItem, stackLayout.getTopElement());
	}

}
