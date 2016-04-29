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

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.common.util.DiagnosticChain;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecp.diagnostician.ECPValidator;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;

/**
 * @author jfaltermeier
 *
 */
public class GenderValidator1 extends ECPValidator {

	private int hitCount;

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.diagnostician.ECPValidator#getValidatedEClassifier()
	 */
	@Override
	public Set<EClassifier> getValidatedEClassifier() {
		final Set<EClassifier> classifiers = new LinkedHashSet<EClassifier>();
		classifiers.add(BowlingPackage.eINSTANCE.getGender());
		return classifiers;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.diagnostician.ECPValidator#validate(org.eclipse.emf.ecore.EDataType, java.lang.Object,
	 *      org.eclipse.emf.common.util.DiagnosticChain, java.util.Map)
	 */
	@Override
	public boolean validate(EDataType eDataType, Object value, DiagnosticChain diagnostics, Map<Object, Object> context) {
		hitCount++;
		diagnostics.add(createDiagnostic(Diagnostic.INFO, "source", 0, "Gender", new Object[] { value }, context));
		return false;
	}

	public int getHitCount() {
		return hitCount;
	}

}
