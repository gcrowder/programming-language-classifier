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
package org.eclipse.emf.ecp.ui.validation.test;

import static org.junit.Assert.assertEquals;

import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecp.diagnostician.ECPDiagnostician;
import org.eclipse.emf.ecp.internal.ui.validation.ValidationTreeViewerFactory;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.swt.widgets.TreeItem;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class ValidationTreeViewer_PTest {

	private TreeViewer treeViewer;

	@Before
	public void before() {
		TestDoubleClickListener.resetHitCount();
		treeViewer = createTreeViewer();
	}

	@After
	public void after() {
		treeViewer.getTree().dispose();
	}

	@Test
	public void testEmpty() {
		final Tree tree = treeViewer.getTree();
		assertEquals(0, tree.getItemCount());
		assertEquals(3, tree.getColumnCount());
		final TreeColumn[] columns = tree.getColumns();
		assertEquals("Description", columns[0].getText());
		assertEquals("EObject", columns[1].getText());
		assertEquals("Structural Feature", columns[2].getText());
	}

	@Test
	public void testInitSingleErrorExpanded() {
		final Diagnostic diagnostic = validateLeague(leagueWithPlayers(0), LeagueValidator.MODE_ONLY_LEAGUE);
		treeViewer.setInput(diagnostic);
		treeViewer.expandAll();
		final Tree tree = treeViewer.getTree();
		assertEquals(1, tree.getItemCount());
		final TreeItem item = tree.getItem(0);
		assertEquals("There is something wrong with the players", item.getText(0));
		assertEquals("League", item.getText(1));
		assertEquals("players", item.getText(2));
		assertEquals(0, item.getItemCount());
	}

	@Test
	public void testInitLeagueWith2PlayersExpanded() {
		final Diagnostic diagnostic = validateLeague(leagueWithPlayers(2), LeagueValidator.MODE_ALL);
		treeViewer.setInput(diagnostic);
		treeViewer.expandAll();
		final Tree tree = treeViewer.getTree();
		// assertEquals(1, tree.getItemCount());
		final TreeItem item = tree.getItem(0);
		assertEquals("There is something wrong with the players", item.getText(0));
		assertEquals("League", item.getText(1));
		assertEquals("players", item.getText(2));
		assertEquals(2, item.getItemCount());
		final TreeItem item21 = item.getItem(0);
		assertEquals("There is something wrong with this Player", item21.getText(0));
		assertEquals("Player 1", item21.getText(1));
		assertEquals("", item21.getText(2));
		assertEquals(0, item21.getItemCount());
		final TreeItem item22 = item.getItem(1);
		assertEquals("There is something wrong with this Player", item22.getText(0));
		assertEquals("Player 2", item22.getText(1));
		assertEquals("", item22.getText(2));
		assertEquals(0, item22.getItemCount());
	}

	@Test
	public void testInitMultipleDiagnosticsExpanded() {
		final Diagnostic diagnostic = validateLeague(leagueWithPlayers(3), LeagueValidator.MODE_ONLY_PLAYER);
		treeViewer.setInput(diagnostic);
		treeViewer.expandAll();
		final Tree tree = treeViewer.getTree();
		// assertEquals(3, tree.getItemCount());
		final TreeItem item1 = tree.getItem(0);
		assertEquals("There is something wrong with this Player", item1.getText(0));
		assertEquals("Player 1", item1.getText(1));
		assertEquals("", item1.getText(2));
		assertEquals(0, item1.getItemCount());
		final TreeItem item2 = tree.getItem(1);
		assertEquals("There is something wrong with this Player", item2.getText(0));
		assertEquals("Player 2", item2.getText(1));
		assertEquals("", item2.getText(2));
		assertEquals(0, item2.getItemCount());
		final TreeItem item3 = tree.getItem(2);
		assertEquals("There is something wrong with this Player", item3.getText(0));
		assertEquals("Player 3", item3.getText(1));
		assertEquals("", item3.getText(2));
		assertEquals(0, item3.getItemCount());
	}

	@Ignore
	@Test
	public void testDoubleClickListener() {
		final Diagnostic diagnostic = validateLeague(leagueWithPlayers(2), LeagueValidator.MODE_ALL);
		treeViewer.setInput(diagnostic);
		treeViewer.expandAll();
		final Tree tree = treeViewer.getTree();
		final Listener[] listeners = tree.getListeners(SWT.MouseDoubleClick);
		assertEquals(1, listeners.length);
		final Listener listener = listeners[0];
		final Event event = new Event();
		event.type = SWT.MouseDoubleClick;
		event.widget = tree;
		listener.handleEvent(event);
		assertEquals(1, TestDoubleClickListener.getHitCount());
	}

	private TreeViewer createTreeViewer() {
		return ValidationTreeViewerFactory.createValidationViewer(new Shell());
	}

	private Diagnostic validateLeague(League league, int mode) {
		final Map<Object, Object> context = new LinkedHashMap<Object, Object>();
		context.put(LeagueValidator.LEAGUE_VALIDATOR_MODE, mode);
		return ECPDiagnostician.INSTANCE.validate(league, context);
	}

	private League leagueWithPlayers(int playerCount) {
		final League league = BowlingFactory.eINSTANCE.createLeague();
		for (int i = 0; i < playerCount; i++) {
			final Player player = BowlingFactory.eINSTANCE.createPlayer();
			player.setName(String.valueOf(i + 1));
			league.getPlayers().add(player);
		}
		return league;
	}

}
