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

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.diagnostician.ECPValidator;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Player;

/**
 * @author jfaltermeier
 *
 */
public class LeagueValidator extends ECPValidator {

	public static final String LEAGUE_VALIDATOR_MODE = "leagueValidatorMode";

	public static final int MODE_ONLY_LEAGUE = 0;
	public static final int MODE_ONLY_PLAYER = 1;
	public static final int MODE_ALL = 2;

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.diagnostician.ECPValidator#getValidatedEClassifier()
	 */
	@Override
	public Set<EClassifier> getValidatedEClassifier() {
		final Set<EClassifier> classifiers = new LinkedHashSet<EClassifier>();
		classifiers.add(BowlingPackage.eINSTANCE.getLeague());
		return classifiers;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.diagnostician.ECPValidator#validate(org.eclipse.emf.ecore.EClass,
	 *      org.eclipse.emf.ecore.EObject, org.eclipse.emf.common.util.DiagnosticChain, java.util.Map)
	 */
	@Override
	public boolean validate(EClass eClass, EObject eObject, DiagnosticChain diagnostics, Map<Object, Object> context) {
		if (!context.containsKey(LEAGUE_VALIDATOR_MODE)) {
			return super.validate(eClass, eObject, diagnostics, context);
		}
		final int mode = (Integer) context.get(LEAGUE_VALIDATOR_MODE);
		switch (mode) {
		case MODE_ONLY_LEAGUE:
			return validateLeague(eClass, eObject, diagnostics, context);
		case MODE_ONLY_PLAYER:
			return validatePlayers(eClass, eObject, diagnostics, context);
		case MODE_ALL:
			return validateAll(eClass, eObject, diagnostics, context);
		default:
			return super.validate(eClass, eObject, diagnostics, context);
		}
	}

	private boolean validateLeague(EClass eClass, EObject eObject, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		final Diagnostic diagnostic = createDiagnostic(Diagnostic.ERROR, "source", 0,
			"There is something wrong with the players",
			new Object[] { eObject, BowlingPackage.eINSTANCE.getLeague_Players() }, context);
		diagnostics.add(diagnostic);
		return false;
	}

	private boolean validatePlayers(EClass eClass, EObject eObject, DiagnosticChain diagnostics,
		Map<Object, Object> context) {
		final League league = (League) eObject;
		final EList<Player> players = league.getPlayers();
		for (final Player p : players) {
			diagnostics.add(createPlayerDiagnostic(p, context));
		}
		return false;
	}

	private boolean validateAll(EClass eClass, EObject eObject, DiagnosticChain diagnostics, Map<Object, Object> context) {
		final League league = (League) eObject;
		final EList<Player> players = league.getPlayers();
		final List<Diagnostic> childDiagnostics = new ArrayList<Diagnostic>();
		for (final Player p : players) {
			childDiagnostics.add(createPlayerDiagnostic(p, context));
		}
		final Diagnostic diagnostic = createDiagnostic("source", 0, "There is something wrong with the players",
			new Object[] { league, BowlingPackage.eINSTANCE.getLeague_Players() }, context, childDiagnostics);
		diagnostics.add(diagnostic);
		return false;
	}

	private Diagnostic createPlayerDiagnostic(Player player, Map<Object, Object> context) {
		return createDiagnostic(Diagnostic.ERROR, "source", 0, "There is something wrong with this Player",
			new Object[] { player }, context);
	}

}
