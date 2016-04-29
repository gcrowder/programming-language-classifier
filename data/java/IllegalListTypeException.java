/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.dynamictree.model.impl;

import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;

/**
 * This {@link IllegalListTypeException} is thrown by a {@link DynamicContainmentTreeDMRConverter} when the base
 * feature of a {@link org.eclipse.emf.ecp.view.dynamictree.model.DynamicContainmentTreeDomainModelReference
 * DynamicContainmentTreeDomainModelReference} is not a list or the list is not of the type
 * {@link org.eclipse.emf.ecore.EReference EReference} .
 *
 * @author Lucas Koehler
 *
 */
public class IllegalListTypeException extends DatabindingFailedException {

	private static final long serialVersionUID = 5721328492316894438L;

	/**
	 * Creates a new {@link IllegalListTypeException} with the given message.
	 *
	 * @param message The message text of the exception
	 */
	public IllegalListTypeException(String message) {
		super(message);
	}
}
