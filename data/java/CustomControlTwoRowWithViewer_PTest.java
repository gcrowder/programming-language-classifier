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
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomControl;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomDomainModelReference;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author jfaltermeier
 * 
 */
@RunWith(DatabindingClassRunner.class)
public class CustomControlTwoRowWithViewer_PTest {

	private Composite parent;
	private CustomControlStub3 customControl;
	private Label label;
	private Table table;
	private League league;
	private Player player;
	private VCustomControl controlModel;

	@Before
	public void before() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		league = BowlingFactory.eINSTANCE.createLeague();
		player = BowlingFactory.eINSTANCE.createPlayer();
		player.setName("Hans");
		player.setDateOfBirth(new Date());
		league.getPlayers().add(player);
		controlModel = VCustomFactory.eINSTANCE.createCustomControl();
		controlModel.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		controlModel.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub3");
		final VCustomDomainModelReference customDomainModelReference = VCustomFactory.eINSTANCE
			.createCustomDomainModelReference();
		customDomainModelReference.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		customDomainModelReference.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub3");
		controlModel.setDomainModelReference(customDomainModelReference);

		// final VHardcodedDomainModelReference hardcodedDomainModelRef = VCustomFactory.eINSTANCE
		// .createHardcodedDomainModelReference();
		// hardcodedDomainModelRef.setControlId("org.eclipse.emf.ecp.view.custom.ui.swt.test.CustomControlStub3");
		// control.setDomainModelReference(hardcodedDomainModelRef);

		customControl = new CustomControlStub3();
		customControl.init(controlModel, ViewModelContextFactory.INSTANCE.createViewModelContext(controlModel, league));
		parent = new Composite(SWTViewTestHelper.createShell(), SWT.NONE);

		// for (final VDomainModelReference modelReference : customControl.getNeededDomainModelReferences()) {
		// modelReference.resolve(league);
		// }

		label = (Label) customControl.renderControl(new SWTGridCell(0, 0, null), parent);
		Composite composite = (Composite) customControl.renderControl(new SWTGridCell(0, 1, null),
			parent);
		// final List<RenderingResultRow<Control>> rows = customControl.createControls(parent);
		// final DoubleColumnRow doubleRow = (DoubleColumnRow) rows.get(0);
		// label = (Label) doubleRow.getLeftControl();
		// Composite composite = (Composite) doubleRow.getRightControl();
		composite = (Composite) composite.getChildren()[0];
		table = (Table) composite.getChildren()[0];
	}

	@Test
	public void testViewerInit() {
		assertEquals(1, table.getItemCount());
		assertEquals(player, table.getItem(0).getData());
		assertEquals(player.getName(), table.getItem(0).getText());
	}

	@Test
	public void testViewerBinding() {
		final Player player2 = BowlingFactory.eINSTANCE.createPlayer();
		player2.setName("Sepp");
		league.getPlayers().add(player2);
		assertEquals(2, table.getItemCount());
		assertEquals(player2, table.getItem(1).getData());
		assertEquals(player2.getName(), table.getItem(1).getText());
		league.getPlayers().remove(0);
		assertEquals(1, table.getItemCount());
		assertEquals(player2, table.getItem(0).getData());
		assertEquals(player2.getName(), table.getItem(0).getText());
	}

	private Map<SWTGridCell, Control> createControlMap(Control... controls) {
		final Map<SWTGridCell, Control> result = new LinkedHashMap<SWTGridCell, Control>();
		for (int i = 0; i < controls.length; i++) {
			result.put(new SWTGridCell(0, i, null), controls[i]);
		}
		return result;
	}

	@Test
	public void testSetEditable() {
		controlModel.setEnabled(false);

		customControl.applyEnable(createControlMap(label, table));
		// customControl.setEditable(false);
		assertFalse(label.isEnabled());
		assertFalse(table.isEnabled());
		// customControl.setEditable(true);
		controlModel.setEnabled(true);
		customControl.applyEnable(createControlMap(label, table));
		assertTrue(label.isEnabled());
		assertTrue(table.isEnabled());
	}

	@Test
	public void testChangeListener() {
		league.getPlayers().add(BowlingFactory.eINSTANCE.createPlayer());
		assertEquals(CustomControlStub3.CHANGE_NOTICED, label.getText());
	}

	@Test
	public void testChangeListenerDispose() {
		assertEquals(3, league.eAdapters().size());
		customControl.dispose();
		assertEquals(2, league.eAdapters().size());
	}
}
