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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.internal.ui.validation.ECPValidationResultServiceImpl;
import org.eclipse.emf.ecp.ui.validation.ECPValidationResultService.ECPValidationResultServiceListener;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.League;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.Before;
import org.junit.Test;

public class ValidationResultService_Test {

	private ECPValidationResultServiceImpl service;
	private Player player;
	private League league;
	private ECPTestValidationResultListener listener;

	@Before
	public void before() {
		service = new ECPValidationResultServiceImpl();
		player = BowlingFactory.eINSTANCE.createPlayer();
		league = BowlingFactory.eINSTANCE.createLeague();
		listener = new ECPTestValidationResultListener();
	}

	@Test
	public void registerForAll() {
		service.register(listener);
		assertNull(listener.getDiagnostic());

		service.setResult(diagnostic(league));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();

		service.setResult(diagnostic(player));
		assertNotNull(listener.getDiagnostic());
	}

	@Test
	public void registerForAllClassifier() {
		service.register(listener, classifiers(true, true));
		assertNull(listener.getDiagnostic());

		service.setResult(diagnostic(league));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();

		service.setResult(diagnostic(player));
		assertNotNull(listener.getDiagnostic());
	}

	@Test
	public void registerForSpecificClassifier() {
		service.register(listener, classifiers(false, true));
		assertNull(listener.getDiagnostic());

		service.setResult(diagnostic(league));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();

		service.setResult(diagnostic(player));
		assertNull(listener.getDiagnostic());
	}

	@Test
	public void reregisterFromAllToSpecific() {
		service.register(listener);
		service.register(listener, classifiers(true, false));

		service.setResult(diagnostic(player));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();
		service.setResult(diagnostic(league));
		assertNull(listener.getDiagnostic());
	}

	@Test
	public void reregisterFromSpecificToAllToOtherSpecific() {
		service.register(listener, classifiers(true, false));
		service.register(listener);

		service.setResult(diagnostic(player));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();
		service.setResult(diagnostic(league));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();

		service.register(listener, classifiers(false, true));

		service.setResult(diagnostic(player));
		assertNull(listener.getDiagnostic());
		service.setResult(diagnostic(league));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();
	}

	@Test
	public void reregisterForMore() {
		service.register(listener, classifiers(true, false));
		service.register(listener, classifiers(true, true));

		service.setResult(diagnostic(player));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();
		service.setResult(diagnostic(league));
		assertNotNull(listener.getDiagnostic());
		listener.resetDiagnostic();
	}

	@Test
	public void deregisterFromAll() {
		service.register(listener);
		service.deregister(listener);

		service.setResult(diagnostic(player));
		assertNull(listener.getDiagnostic());
		service.setResult(diagnostic(league));
		assertNull(listener.getDiagnostic());
	}

	@Test
	public void deregisterFromSpecific() {
		service.register(listener, classifiers(true, false));
		service.deregister(listener);

		service.setResult(diagnostic(player));
		assertNull(listener.getDiagnostic());
		service.setResult(diagnostic(league));
		assertNull(listener.getDiagnostic());
	}

	@Test
	public void testResultSingle() {
		service.register(listener);
		final Diagnostic diagnostic = diagnostic(league);
		service.setResult(diagnostic);
		assertEquals(diagnostic, listener.getDiagnostic());
	}

	@Test
	public void testResultList() {
		service.register(listener);
		final List<Diagnostic> list = new ArrayList<Diagnostic>();
		list.add(diagnostic(league));
		list.add(diagnostic(player));
		service.setResult(list);
		assertEquals(list, listener.getDiagnostic());
	}

	@Test
	public void testResultArray() {
		service.register(listener);
		final Diagnostic[] array = new Diagnostic[2];
		array[0] = diagnostic(league);
		array[1] = diagnostic(player);
		service.setResult(array);
		assertEquals(array, listener.getDiagnostic());
	}

	private Set<EClassifier> classifiers(boolean player, boolean league) {
		final Set<EClassifier> set = new LinkedHashSet<EClassifier>();
		if (player) {
			set.add(BowlingPackage.eINSTANCE.getPlayer());
		}
		if (league) {
			set.add(BowlingPackage.eINSTANCE.getLeague());
		}
		return set;
	}

	private Diagnostic diagnostic(EObject object) {
		return createDiagnostic(Diagnostic.ERROR, "source", 0, "There is something wrong",
			new Object[] { object }, new LinkedHashMap<Object, Object>());
	}

	private Diagnostic createDiagnostic(int severity, String source, int code, String message, Object[] data,
		Map<Object, Object> context) {
		return new BasicDiagnostic(severity, source, code, message, data);
	}

	private class ECPTestValidationResultListener implements ECPValidationResultServiceListener {

		private Object diagnostic;

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.ui.validation.ECPValidationResultService.ECPValidationResultServiceListener#resultChanged(java.lang.Object)
		 */
		@Override
		public void resultChanged(Object diagnostic) {
			this.diagnostic = diagnostic;
		}

		public Object getDiagnostic() {
			return diagnostic;
		}

		public void resetDiagnostic() {
			diagnostic = null;
			assertNull(diagnostic);
		}

	}

}
