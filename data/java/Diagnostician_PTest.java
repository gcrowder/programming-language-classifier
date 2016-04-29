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
package org.eclipse.emf.ecp.validation.diagnostician.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecp.diagnostician.ECPDiagnostician;
import org.eclipse.emf.ecp.diagnostician.ECPValidator;
import org.eclipse.emf.ecp.internal.diagnostician.ClassifierValidatorWrapper;
import org.eclipse.emf.ecp.internal.diagnostician.PackageValidatorWrapper;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.Test;

public class Diagnostician_PTest {

	@Test
	public void testClassifierWrapperEClass() {
		final Set<ECPValidator> validators = new LinkedHashSet<ECPValidator>();
		validators.add(new PlayerValidatorHeight());
		validators.add(new PlayerValidatorName());
		validators.add(new PlayerValidatorOK());
		final ClassifierValidatorWrapper wrapper = new ClassifierValidatorWrapper(BowlingPackage.eINSTANCE.getPlayer(),
			validators);
		final BasicDiagnostic chain = new BasicDiagnostic();
		assertFalse(wrapper.validate(player(), chain, new LinkedHashMap<Object, Object>()));
		assertEquals(Diagnostic.ERROR, chain.getSeverity());
		assertEquals(2, chain.getChildren().size());
		assertEquals(Diagnostic.WARNING, chain.getChildren().get(0).getSeverity());
		assertEquals("Height", chain.getChildren().get(0).getMessage());
		assertEquals(Diagnostic.ERROR, chain.getChildren().get(1).getSeverity());
		assertEquals("Name", chain.getChildren().get(1).getMessage());
	}

	@Test
	public void testClassifierWrapperEDataType() {
		final Set<ECPValidator> validators = new LinkedHashSet<ECPValidator>();
		validators.add(new GenderValidator1());
		validators.add(new GenderValidator2());
		validators.add(new GenderValidator3());
		final ClassifierValidatorWrapper wrapper = new ClassifierValidatorWrapper(BowlingPackage.eINSTANCE.getGender(),
			validators);
		final BasicDiagnostic chain = new BasicDiagnostic();
		assertFalse(wrapper.validate(BowlingPackage.eINSTANCE.getGender(), Gender.FEMALE, chain,
			new LinkedHashMap<Object, Object>()));
		assertEquals(Diagnostic.CANCEL, chain.getSeverity());
		assertEquals(2, chain.getChildren().size());
		assertEquals(Diagnostic.INFO, chain.getChildren().get(0).getSeverity());
		assertEquals("Gender", chain.getChildren().get(0).getMessage());
		assertEquals(Diagnostic.CANCEL, chain.getChildren().get(1).getSeverity());
		assertEquals("Cancel", chain.getChildren().get(1).getMessage());
	}

	@Test
	public void testPackageWrapperEClass() {
		final PlayerValidatorName validator = new PlayerValidatorName();
		final Map<EClassifier, ECPValidator> classifierToValidatorMap = new LinkedHashMap<EClassifier, ECPValidator>();
		classifierToValidatorMap.put(BowlingPackage.eINSTANCE.getPlayer(), validator);
		final PackageValidatorWrapper packageValidatorWrapper = new PackageValidatorWrapper(classifierToValidatorMap);
		assertEquals(0, validator.getHitCount());
		assertFalse(packageValidatorWrapper.validate(player(), new BasicDiagnostic(),
			new LinkedHashMap<Object, Object>()));
		assertEquals(1, validator.getHitCount());
		assertFalse(packageValidatorWrapper.validate(BowlingPackage.eINSTANCE.getPlayer(), player(),
			new BasicDiagnostic(),
			new LinkedHashMap<Object, Object>()));
		assertEquals(2, validator.getHitCount());
	}

	@Test
	public void testPackageWrapperEDataType() {
		final GenderValidator1 validator = new GenderValidator1();
		final Map<EClassifier, ECPValidator> classifierToValidatorMap = new LinkedHashMap<EClassifier, ECPValidator>();
		classifierToValidatorMap.put(BowlingPackage.eINSTANCE.getGender(), validator);
		final PackageValidatorWrapper packageValidatorWrapper = new PackageValidatorWrapper(classifierToValidatorMap);
		assertEquals(0, validator.getHitCount());
		assertFalse(packageValidatorWrapper.validate(BowlingPackage.eINSTANCE.getGender(), Gender.FEMALE,
			new BasicDiagnostic(), new LinkedHashMap<Object, Object>()));
		assertEquals(1, validator.getHitCount());
		assertFalse(packageValidatorWrapper.validate(BowlingPackage.eINSTANCE.getGender(), Gender.FEMALE,
			new BasicDiagnostic(), new LinkedHashMap<Object, Object>()));
		assertEquals(2, validator.getHitCount());
	}

	@Test
	public void testDiagnostician() {
		final Player player = player();
		assertTrue(ECPDiagnostician.INSTANCE.canValidate(player));
		final Diagnostic diagnostic1 = ECPDiagnostician.INSTANCE.validate(player);
		assertEquals(Diagnostic.ERROR, diagnostic1.getSeverity());
		assertEquals(2, diagnostic1.getChildren().size());
		assertEquals(Diagnostic.WARNING, diagnostic1.getChildren().get(0).getSeverity());
		assertEquals("Height", diagnostic1.getChildren().get(0).getMessage());
		assertEquals(Diagnostic.ERROR, diagnostic1.getChildren().get(1).getSeverity());
		assertEquals("Name", diagnostic1.getChildren().get(1).getMessage());
		final Diagnostic diagnostic2 = ECPDiagnostician.INSTANCE.validate(player, new LinkedHashMap<Object, Object>());
		assertEquals(Diagnostic.ERROR, diagnostic2.getSeverity());
		assertEquals(2, diagnostic2.getChildren().size());
		assertEquals(Diagnostic.WARNING, diagnostic2.getChildren().get(0).getSeverity());
		assertEquals("Height", diagnostic2.getChildren().get(0).getMessage());
		assertEquals(Diagnostic.ERROR, diagnostic2.getChildren().get(1).getSeverity());
		assertEquals("Name", diagnostic2.getChildren().get(1).getMessage());
	}

	private Player player() {
		return BowlingFactory.eINSTANCE.createPlayer();
	}

}
