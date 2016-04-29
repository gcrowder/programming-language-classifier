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
package org.eclipse.emf.ecp.view.ui.editor.test.controls;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.eclipse.emf.common.command.BasicCommandStack;
import org.eclipse.emf.common.notify.AdapterFactory;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.table.ui.swt.test.SWTTable_PTest;
import org.eclipse.emf.ecp.view.test.common.spi.GCCollectable;
import org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.provider.ComposedAdapterFactory;
import org.eclipse.emf.edit.provider.ReflectiveItemProviderAdapterFactory;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.emf.emfstore.bowling.TournamentType;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotButton;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotDateTime;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotShell;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTable;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.junit.AfterClass;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * SWTBotTest for table control.
 *
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class TableControlSWTBot_PTest extends ECPCommonSWTBotTest {

	private static double memBefore;
	private static double memAfter;

	private final boolean isDomainCollectable;

	private GCCollectable viewCollectable;
	private GCCollectable domainCollectable;
	private ComposedAdapterFactory adapterFactory;

	public TableControlSWTBot_PTest(boolean isDomainCollectable) {
		this.isDomainCollectable = isDomainCollectable;
	}

	@AfterClass
	public static void afterClass() {
		final double diff = Math.abs((memBefore - memAfter) / memBefore);
		assertTrue(diff < 0.05);
	}

	@Parameters
	public static Collection<Object[]> data() {
		final List<Object[]> data = new ArrayList<Object[]>();
		for (int i = 0; i < 24; i++) {
			data.add(new Boolean[] { false });
		}
		data.add(new Boolean[] { true });
		return data;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#logic()
	 */
	@Override
	public void logic() {
		final SWTBotButton addButton = bot.button(0);
		addButton.click();
		addButton.click();

		final SWTBotTable table = bot.table();

		table.click(0, 1);
		SWTBotText text = bot.text();
		text.setFocus();
		text.setText("Maxl Morlock");

		table.click(0, 2);
		final SWTBotDateTime dateTime = bot.dateTime();
		dateTime.setFocus();
		dateTime.setDate(new Date());

		table.click(0, 3);
		// no actual cell editor to test (boolean)

		table.click(0, 4);
		text = bot.text();
		text.setFocus();
		text.setText("maxl@foo.bar");

		table.click(1, 1);
		text = bot.text();
		text.setFocus();
		text.setText("Hans Kalb");

		table.click(2, 1);
		text = bot.text();
		text.setFocus();
		text.setText("Heiner Stuhlfauth");

		table.click(2, 1);
		final SWTBotButton deleteButton = bot.button(1);
		deleteButton.click();

		final SWTBotShell shell = bot.activeShell();
		final SWTBotButton okButton = shell.bot().button(0);
		okButton.click();
	}

	@Override
	public void assertions(double before, double after) {
		TableControlSWTBot_PTest.memBefore += before;
		TableControlSWTBot_PTest.memAfter += after;

		if (getDomainObject() != null) {
			assertTrue("More than four adapter left on domain model element after dispose of ECPSWTView: "
				+ getDomainObject().eAdapters().size()
				+ " adapters. Not all adapters can be removed, but it's maybe time to get suspicious.",
				getDomainObject()
					.eAdapters().size() < 5);
			assertTrue("More than four adapter left on domain model element after dispose of ECPSWTView: "
				+ ((League) getDomainObject()).getPlayers().get(0).eAdapters().size()
				+ " adapters. Not all adapters can be removed, but it's maybe time to get suspicious.",
				((League) getDomainObject()).getPlayers().get(0).eAdapters().size() < 5);
		}

		assertTrue(getSWTViewCollectable().isCollectable());
		unsetSWTViewCollectable();
		unsetDomainObject();
		adapterFactory.dispose();
		assertTrue(viewCollectable.isCollectable());
		viewCollectable = null;
		assertTrue(domainCollectable.isCollectable());

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#createDomainObject()
	 */
	@Override
	public EObject createDomainObject() {
		League league = (League) getDomainObject();

		if (isDomainCollectable) {
			// remove reference to domain object, since gc will be tested
			setDomainObject(null);
		}

		if (league == null) {
			league = BowlingFactory.eINSTANCE.createLeague();
			league.setName("Bayern Nord");
			final Player player = createPlayer();
			league.getPlayers().add(player);
			final ResourceSet resourceSet = new ResourceSetImpl();
			final Resource resource = resourceSet.createResource(URI.createFileURI("foo.xmi"));
			resource.getContents().add(league);
			resource.getContents().add(player);
			addEditingDomain(resourceSet);
			memBefore = 0d;
			memAfter = 0d;
		} else {
			final Player player = league.getPlayers().get(0);
			player.setName("Max Morlock");
			player.setHeight(1.8);
			player.getEMails().clear();
			player.getEMails().add("maxl@foobar.com");
			player.setNumberOfVictories(249);
			player.getPlayedTournamentTypes().clear();
			player.getPlayedTournamentTypes().add(TournamentType.AMATEUR);
			player.setWinLossRatio(new BigDecimal(0.8));
			player.setGender(Gender.MALE);
			league.getPlayers().remove(1);
		}

		if (!isDomainCollectable) {
			setDomainObject(league);
		}

		domainCollectable = new GCCollectable(league);
		return league;
	}

	private Player createPlayer() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		player.setName("Max Morlock");
		final Calendar calendar = Calendar.getInstance();
		calendar.clear();
		calendar.set(11, 5, 1925);
		player.setDateOfBirth(calendar.getTime());
		player.setHeight(1.80d);
		player.setIsProfessional(true);
		player.getEMails().add("maxl@foobar.com");
		player.setNumberOfVictories(249);
		player.getPlayedTournamentTypes().add(TournamentType.AMATEUR);
		player.setWinLossRatio(new BigDecimal(0.8));
		player.setGender(Gender.MALE);
		return player;
	}

	private void addEditingDomain(ResourceSet resourceSet) {
		adapterFactory = new ComposedAdapterFactory(
			ComposedAdapterFactory.Descriptor.Registry.INSTANCE);
		adapterFactory = new ComposedAdapterFactory(new AdapterFactory[] { adapterFactory,
			new ReflectiveItemProviderAdapterFactory() });
		final AdapterFactoryEditingDomain domain = new AdapterFactoryEditingDomain(adapterFactory,
			new BasicCommandStack(), resourceSet);
		resourceSet.eAdapters().add(new AdapterFactoryEditingDomain.EditingDomainProvider(domain));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.ui.editor.test.ECPCommonSWTBotTest#createView()
	 */
	@Override
	public VView createView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getGame());
		createTableControl(view);
		viewCollectable = new GCCollectable(view);
		return view;
	}

	/**
	 * @param view
	 */
	private void createTableControl(VView view) {
		final VTableControl control = VTableFactory.eINSTANCE.createTableControl();

		final VTableDomainModelReference domainRef = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		domainRef.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Players());
		control.setDomainModelReference(domainRef);

		final VDomainModelReference nameColumn = SWTTable_PTest.createTableColumn(BowlingPackage.eINSTANCE
			.getPlayer_Name());

		final VDomainModelReference birthColumn = SWTTable_PTest.createTableColumn(BowlingPackage.eINSTANCE
			.getPlayer_DateOfBirth());

		final VDomainModelReference professionalColumn = SWTTable_PTest.createTableColumn(BowlingPackage.eINSTANCE
			.getPlayer_IsProfessional());

		final VDomainModelReference eMailsColumn = SWTTable_PTest.createTableColumn(BowlingPackage.eINSTANCE
			.getPlayer_EMails());

		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(nameColumn);
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(birthColumn);
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(professionalColumn);
		VTableDomainModelReference.class.cast(control.getDomainModelReference()).getColumnDomainModelReferences()
			.add(eMailsColumn);

		view.getChildren().add(control);
	}

}
