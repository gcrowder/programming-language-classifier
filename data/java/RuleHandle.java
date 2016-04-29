/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.rule.model.Rule;

/**
 * @author Jonas
 *
 */
public class RuleHandle {

	private final Rule rule;
	private final VElement parent;
	private final EObject domainObject;

	/**
	 * Constructor.
	 *
	 * @param rule rule
	 * @param renderable renderable
	 * @param domainObject domainObject.
	 */
	public RuleHandle(Rule rule, VElement renderable, EObject domainObject) {
		this.rule = rule;
		parent = renderable;
		this.domainObject = domainObject;
	}

	/**
	 * @return the rule
	 */
	public Rule getRule() {
		return rule;
	}

	/**
	 * @return the parent
	 */
	public VElement getParent() {
		return parent;
	}

	/**
	 * @return the domainObject
	 */
	public EObject getDomainObject() {
		return domainObject;
	}

}
