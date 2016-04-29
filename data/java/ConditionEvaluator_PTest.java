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
 ******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.rule.model.LeafCondition;
import org.eclipse.emf.ecp.view.spi.rule.model.RuleFactory;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Game;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Matchup;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.emf.emfstore.bowling.Tournament;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author Eugen Neufeld
 *
 */
public class ConditionEvaluator_PTest {

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	private static final String CORRECT_PLAYER_NAME = "player";
	private static final Gender CORRECT_PLAYER_GENDER = Gender.FEMALE;
	private static Date correctPlayerBirthDate;

	private static final int CORRECT_PLAYER_VICTORIES = 42;
	private static final double CORRECT_PLAYER_HEIGHT = 42.42;
	private static final boolean CORRECT_PLAYER_PROFESSIONAL = false;
	private static final BigDecimal CORRECT_PLAYER_RATION = new BigDecimal(42);

	private static final String CORRECT_PLAYER_EMAIL1 = "asdf@asdf.com";
	private static final String CORRECT_PLAYER_EMAIL2 = "player@asdf.com";

	static {
		try {
			correctPlayerBirthDate = DateFormat.getDateInstance(DateFormat.SHORT, Locale.GERMANY).parse("11.11.2011");
		} catch (final ParseException ex) {
			ex.printStackTrace();
		}
	}

	private static Date getDate(String value) {
		try {
			return DateFormat.getDateInstance(DateFormat.SHORT, Locale.GERMANY).parse(value);
		} catch (final ParseException ex) {
			ex.printStackTrace();
		}
		return null;
	}

	private Player setupPlayerRight() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		player.setName(CORRECT_PLAYER_NAME);
		player.setGender(CORRECT_PLAYER_GENDER);
		player.setNumberOfVictories(CORRECT_PLAYER_VICTORIES);
		player.setDateOfBirth(correctPlayerBirthDate);
		player.setHeight(CORRECT_PLAYER_HEIGHT);
		player.setIsProfessional(CORRECT_PLAYER_PROFESSIONAL);
		player.setWinLossRatio(CORRECT_PLAYER_RATION);
		player.getEMails().add(CORRECT_PLAYER_EMAIL1);
		player.getEMails().add(CORRECT_PLAYER_EMAIL2);
		return player;
	}

	private League setupLeague() {
		final League league = BowlingFactory.eINSTANCE.createLeague();
		final Player p1 = BowlingFactory.eINSTANCE.createPlayer();
		p1.setName(CORRECT_PLAYER_NAME + "1");
		final Player p2 = BowlingFactory.eINSTANCE.createPlayer();
		p2.setName(CORRECT_PLAYER_NAME + "2");
		league.getPlayers().add(p1);
		league.getPlayers().add(p2);
		return league;
	}

	private LeafCondition setupLeafCondition(EStructuralFeature domainFeature, Object expectedValue,
		EObject resolveObject) {
		// final LeafCondition leafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		// final VFeaturePathDomainModelReference modelReference = ViewFactory.eINSTANCE
		// .createVFeaturePathDomainModelReference();
		// modelReference.setDomainModelEFeature(domainFeature);
		// leafCondition.setDomainModelReference(modelReference);
		// leafCondition.setExpectedValue(expectedValue);
		// modelReference.resolve(resolveObject);
		// return leafCondition;
		final List<EReference> references = Collections.emptyList();
		return setupLeafCondition(domainFeature, expectedValue, resolveObject, references);
	}

	private LeafCondition setupLeafCondition(EStructuralFeature domainFeature, Object expectedValue,
		EObject resolveObject, List<EReference> eReferences) {
		final LeafCondition leafCondition = RuleFactory.eINSTANCE.createLeafCondition();
		final VFeaturePathDomainModelReference modelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		modelReference.setDomainModelEFeature(domainFeature);
		leafCondition.setDomainModelReference(modelReference);
		leafCondition.setExpectedValue(expectedValue);
		modelReference.getDomainModelEReferencePath().addAll(eReferences);
		return leafCondition;
	}

	private LeafCondition setupLeafConditionWithValueDMR(EObject domain, Object expectedValue, EReference domainRef,
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

	@Test
	public void testBooleanConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_IsProfessional(),
			CORRECT_PLAYER_PROFESSIONAL, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testBooleanConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_IsProfessional(),
			!CORRECT_PLAYER_PROFESSIONAL, player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testStringConditionNullRight() {
		final Player player = setupPlayerRight();
		player.setName(null);
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Name(),
			null, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testStringConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Name(),
			CORRECT_PLAYER_NAME, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testStringConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Name(),
			CORRECT_PLAYER_NAME + "BLA", player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testIntConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(),
			CORRECT_PLAYER_VICTORIES, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testIntConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_NumberOfVictories(),
			CORRECT_PLAYER_VICTORIES + 1, player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testDoubleConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Height(),
			CORRECT_PLAYER_HEIGHT, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testDoubleConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Height(),
			CORRECT_PLAYER_HEIGHT + 1, player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testDateConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_DateOfBirth(),
			correctPlayerBirthDate, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testDateConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_DateOfBirth(),
			getDate("12.12.2012"), player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testDateConditionSame() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_DateOfBirth(),
			getDate("11.11.2011"), player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testEnumConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Gender(),
			CORRECT_PLAYER_GENDER.getLiteral(), player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testEnumConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_Gender(),
			Gender.MALE.getLiteral(), player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testBigDecimalConditionRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_WinLossRatio(),
			CORRECT_PLAYER_RATION, player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testBigDecimalConditionSame() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_WinLossRatio(),
			new BigDecimal(42), player);

		assertTrue(leafCondition.evaluate(player));
	}

	@Test
	public void testBigDecimalConditionWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_WinLossRatio(),
			new BigDecimal(1), player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testContainmentConditionStringRight() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition1 = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_EMails(),
			CORRECT_PLAYER_EMAIL1, player);

		assertTrue(leafCondition1.evaluate(player));

		final LeafCondition leafCondition2 = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_EMails(),
			CORRECT_PLAYER_EMAIL2, player);

		assertTrue(leafCondition2.evaluate(player));
	}

	@Test
	public void testContainmentConditionStringWrong() {
		final Player player = setupPlayerRight();
		final LeafCondition leafCondition = setupLeafCondition(BowlingPackage.eINSTANCE.getPlayer_EMails(),
			"bla@bla.com", player);

		assertFalse(leafCondition.evaluate(player));
	}

	@Test
	public void testContainmentConditionEObjectRight() {
		final League league = setupLeague();
		for (int i = 1; i <= league.getPlayers().size(); i++) {
			final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR(league, CORRECT_PLAYER_NAME + i++,
				BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
				BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList());

			assertTrue(leafCondition1.evaluate(league));
		}

	}

	@Test
	public void testContainmentConditionEObjectWrong() {
		final League league = setupLeague();
		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR(league, CORRECT_PLAYER_NAME,
			BowlingPackage.eINSTANCE.getLeague_Players(), Collections.<EReference> emptyList(),
			BowlingPackage.eINSTANCE.getPlayer_Name(), Collections.<EReference> emptyList());

		assertFalse(leafCondition1.evaluate(league));

	}

	@Ignore
	public void testContainmentConditionEObjectDeep() {
		/*
		 * Currently ignore this test case because it relied on some "iterator magic" of the feature path dmr that
		 * allowed to have a path with a list in the middle:
		 * Like in this test case the dmr's for the leaf condition have the following scope:
		 * Tournament -> List<Matchup> -> List<Game> -> List<Frame>
		 * <---------------DMR---------------->
		 * ...................................<----valueDMR---->
		 * This was kind of a hack and does not work with the databinding service.
		 */
		final Tournament tournament = BowlingFactory.eINSTANCE.createTournament();
		final Matchup m1 = BowlingFactory.eINSTANCE.createMatchup();
		final Matchup m2 = BowlingFactory.eINSTANCE.createMatchup();
		tournament.getMatchups().add(m1);
		tournament.getMatchups().add(m2);
		final Game g1 = BowlingFactory.eINSTANCE.createGame();
		final Game g2 = BowlingFactory.eINSTANCE.createGame();

		g1.getFrames().add(2);
		g1.getFrames().add(3);
		g2.getFrames().add(5);
		g2.getFrames().add(8);

		m1.getGames().add(g1);
		m1.getGames().add(g2);

		final Game g3 = BowlingFactory.eINSTANCE.createGame();
		final Game g4 = BowlingFactory.eINSTANCE.createGame();

		g3.getFrames().add(13);
		g3.getFrames().add(21);
		g4.getFrames().add(34);
		g4.getFrames().add(55);

		m2.getGames().add(g3);
		m2.getGames().add(g4);

		final LeafCondition leafCondition1 = setupLeafConditionWithValueDMR(tournament, 21,
			BowlingPackage.eINSTANCE.getMatchup_Games(),
			Arrays.asList(BowlingPackage.eINSTANCE.getTournament_Matchups()),
			BowlingPackage.eINSTANCE.getGame_Frames(), Collections.<EReference> emptyList());

		assertTrue(leafCondition1.evaluate(tournament));

	}

	@Test
	public void testContainmentConditionEObjectDeepIndexDMR() {
		final Tournament tournament = BowlingFactory.eINSTANCE.createTournament();
		final Matchup m1 = BowlingFactory.eINSTANCE.createMatchup();
		final Matchup m2 = BowlingFactory.eINSTANCE.createMatchup();
		tournament.getMatchups().add(m1);
		tournament.getMatchups().add(m2);
		final Game g1 = BowlingFactory.eINSTANCE.createGame();
		final Game g2 = BowlingFactory.eINSTANCE.createGame();

		g1.getFrames().add(2);
		g1.getFrames().add(3);
		g2.getFrames().add(5);
		g2.getFrames().add(8);

		m1.getGames().add(g1);
		m1.getGames().add(g2);

		final Game g3 = BowlingFactory.eINSTANCE.createGame();
		final Game g4 = BowlingFactory.eINSTANCE.createGame();

		g3.getFrames().add(13);
		g3.getFrames().add(21);
		g4.getFrames().add(34);
		g4.getFrames().add(55);

		m2.getGames().add(g3);
		m2.getGames().add(g4);

		final VIndexDomainModelReference indexDMR = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		indexDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getTournament_Matchups());
		indexDMR.setIndex(1);
		final VFeaturePathDomainModelReference targetDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		targetDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getMatchup_Games());
		indexDMR.setTargetDMR(targetDMR);

		final VFeaturePathDomainModelReference valueDMR = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		valueDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getGame_Frames());

		final LeafCondition leafCondtion = RuleFactory.eINSTANCE.createLeafCondition();
		leafCondtion.setDomainModelReference(indexDMR);
		leafCondtion.setValueDomainModelReference(valueDMR);
		leafCondtion.setExpectedValue(21);

		assertTrue(leafCondtion.evaluate(tournament));
	}
}
