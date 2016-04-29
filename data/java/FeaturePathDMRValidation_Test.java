/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.model.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EValidator;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.model.util.ViewValidator;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class FeaturePathDMRValidation_Test {

	private VView view;
	private VControl control;
	private VFeaturePathDomainModelReference featurePath;

	private EValidator validator;
	private BasicDiagnostic chain;
	private LinkedHashMap<Object, Object> context;
	private final Boolean createChain;

	public FeaturePathDMRValidation_Test(Boolean createChain) {
		this.createChain = createChain;
	}

	@Parameters
	public static Collection<Object[]> data() {
		// run all tests once with a diagnostic chain and once without
		final List<Object[]> parameters = new ArrayList<Object[]>();
		parameters.add(new Object[] { true });
		parameters.add(new Object[] { false });
		return parameters;
	}

	@Before
	public void before() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		featurePath = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		control.setDomainModelReference(featurePath);

		validator = ViewValidator.INSTANCE;
		context = new LinkedHashMap<Object, Object>();
		if (createChain) {
			chain = new BasicDiagnostic();
		} else {
			chain = null;
		}
	}

	private boolean validate() {
		return validator.validate(featurePath, chain, context);
	}

	private DiagnosticInfo controlDMR() {
		return new DiagnosticInfo(Diagnostic.ERROR, control, VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo controlDMRWarning() {
		return new DiagnosticInfo(Diagnostic.WARNING, control, VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo featurePathEFeature() {
		return new DiagnosticInfo(Diagnostic.ERROR, featurePath, VViewPackage.eINSTANCE
			.getFeaturePathDomainModelReference_DomainModelEFeature());
	}

	private DiagnosticInfo featurePathPath() {
		return new DiagnosticInfo(Diagnostic.ERROR, featurePath, VViewPackage.eINSTANCE
			.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	@Test
	public void testNoEFeature() {
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), featurePathEFeature());
		}
	}

	@Test
	public void testNoEFeatureNoContainer() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(featurePathEFeature());
		}
	}

	@Test
	public void testErrorOnPath() {
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getReferee_League());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), featurePathPath());
		}
	}

	@Test
	public void testErrorOnFeature() {
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), featurePathEFeature());
		}
	}

	@Test
	public void testOkPath() {
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadRootEClass() {
		view.setRootEClass(BowlingPackage.eINSTANCE.getPlayer());
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), featurePathPath());
		}
	}

	@Test
	public void testBadRootEClassOkRootEClassInContext() {
		context.put(ViewValidator.ECLASS_KEY, BowlingPackage.eINSTANCE.getFan());
		view.setRootEClass(BowlingPackage.eINSTANCE.getPlayer());
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
		}
	}

	@Test
	public void testOkPathNoRootEClassOnContainer() {
		view.setRootEClass(null);
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.WARNING, chain.getSeverity());
			assertChain(controlDMRWarning());
		}
	}

	@Test
	public void testOkPathNotInView() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadEFeatureNotInView() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(featurePathEFeature());
		}
	}

	@Test
	public void testBadPathNotInView() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(featurePathPath());
		}
	}

	@Test
	public void testOnlyEFeatureNotInView() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
		featurePath.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	private void assertChain(DiagnosticInfo... infos) {
		final Set<DiagnosticInfo> infoSet = new LinkedHashSet<DiagnosticInfo>(Arrays.asList(infos));
		assertEquals(infos.length, chain.getChildren().size());
		for (final Diagnostic child : chain.getChildren()) {
			boolean found = false;
			final Iterator<DiagnosticInfo> iterator = infoSet.iterator();
			while (iterator.hasNext()) {
				final DiagnosticInfo next = iterator.next();
				if (next.sameData(child)) {
					found = true;
					iterator.remove();
					break;
				}
			}
			if (!found) {
				fail("Chain is missing child diagnostic.");
			}
		}
	}

	private class DiagnosticInfo {
		private final int severity;
		private final EObject object;
		private final EStructuralFeature feature;

		public DiagnosticInfo(int severity, EObject object, EStructuralFeature feature) {
			this.severity = severity;
			this.object = object;
			this.feature = feature;
		}

		public boolean sameData(Diagnostic diagnostic) {
			if (diagnostic.getSeverity() != severity) {
				return false;
			}
			if (!object.equals(diagnostic.getData().get(0))) {
				return false;
			}
			if (!feature.equals(diagnostic.getData().get(1))) {
				return false;
			}
			return true;
		}

	}
}
