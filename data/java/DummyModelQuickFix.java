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

import java.util.HashMap;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.quickfix.ModelQuickFix;
import org.eclipse.emf.ecp.quickfix.ModelQuickFixException;

/**
 * @author Alexandra Buzila
 *
 */
public class DummyModelQuickFix implements ModelQuickFix {

	private double priority;
	private final String problemID;
	/* map from problemID to label */
	private final HashMap<String, String> labels = new HashMap<String, String>();

	/**
	 * Default constructor.
	 *
	 * @param problemID the problem ID
	 */
	public DummyModelQuickFix(String problemID) {
		this.problemID = problemID;
		labels.put(problemID, "Solve problem " + problemID);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.quickfix.ModelQuickFix#isApplicable(org.eclipse.emf.common.util.Diagnostic)
	 */
	@Override
	public double isApplicable(Diagnostic diagnostic) {
		final Object data = diagnostic.getData().get(0);

		if (data != null && data.equals(problemID)) {
			return priority;
		}
		return 0;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.quickfix.ModelQuickFix#applyFix(org.eclipse.emf.ecore.EObject)
	 */
	@Override
	public void applyFix(EObject target) throws ModelQuickFixException {
		// do nothing
	}

	/**
	 * Sets the priority.
	 * 
	 * @param priority the priority
	 */
	public void setPriority(double priority) {
		this.priority = priority;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.quickfix.ModelQuickFix#getLabel(org.eclipse.emf.common.util.Diagnostic)
	 */
	@Override
	public String getLabel(Diagnostic diagnostic) {
		final Object data = diagnostic.getData().get(0);

		if (data != null && data.equals(problemID)) {
			return labels.get(problemID);
		}
		return "";
	}

}
