/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Alexandra Buzila - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.quickfix.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecp.quickfix.ModelQuickFix;
import org.eclipse.emf.ecp.quickfix.internal.ModelQuickFixRegistryImpl;
import org.junit.Test;

public class ModelQuickFixRegistry_Test {

	@Test
	public void registryTest() {
		final String id = "org.eclipse.emf.ecp.quickfix.test.problem_1"; //$NON-NLS-1$

		final ModelQuickFixRegistryImpl registry = new ModelQuickFixRegistryImpl();

		final DummyModelQuickFix dummyQuickFix1 = getDummyQuickFix(id, 1);
		final DummyModelQuickFix dummyQuickFix3 = getDummyQuickFix(id, 2.5);
		final DummyModelQuickFix dummyQuickFix2 = getDummyQuickFix(id, 2);
		final DummyModelQuickFix dummyQuickFix4 = getDummyQuickFix(id, 4);

		registry.registerModelQuickFix(dummyQuickFix2);
		registry.registerModelQuickFix(dummyQuickFix3);
		registry.registerModelQuickFix(dummyQuickFix1);
		registry.registerModelQuickFix(dummyQuickFix4);

		final Diagnostic diagnostic = new BasicDiagnostic(
			"org.eclipse.emf.ecp.ui.quickfix.test", 0, "test", new Object[] { id }); //$NON-NLS-1$ //$NON-NLS-2$

		final List<ModelQuickFix> modelQuickFixes = registry.getAllModelQuickFixes();
		assertTrue(
			"Quick fix not found in the registry", modelQuickFixes.containsAll(Arrays.asList(dummyQuickFix1, dummyQuickFix1, dummyQuickFix1, dummyQuickFix1))); //$NON-NLS-1$

		final List<ModelQuickFix> applicableModelQuickFixes = registry.getApplicableModelQuickFixes(diagnostic);
		assertEquals("", applicableModelQuickFixes.get(0), dummyQuickFix4);
		assertEquals("", applicableModelQuickFixes.get(1), dummyQuickFix3);
		assertEquals("", applicableModelQuickFixes.get(2), dummyQuickFix2);
		assertEquals("", applicableModelQuickFixes.get(3), dummyQuickFix1);

	}

	private DummyModelQuickFix getDummyQuickFix(String id, double priority) {
		final DummyModelQuickFix quickFix = new DummyModelQuickFix(id);
		quickFix.setPriority(priority);
		return quickFix;
	}

}