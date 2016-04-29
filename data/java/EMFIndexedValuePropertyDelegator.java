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

import java.util.List;

import org.eclipse.core.databinding.property.INativePropertyListener;
import org.eclipse.core.databinding.property.ISimplePropertyListener;
import org.eclipse.core.databinding.property.value.IValueProperty;
import org.eclipse.core.databinding.property.value.SimpleValueProperty;

/**
 * This class adds support for addressing specific elements of a list by an index to an encapsulated
 * {@link IValueProperty}.
 *
 * @author Lucas Koehler
 *
 */
public class EMFIndexedValuePropertyDelegator extends SimpleValueProperty {

	private final IValueProperty delegate;
	private final int index;

	/**
	 * Creates a new instance of {@link EMFIndexedValuePropertyDelegator}.
	 *
	 * @param delegate The encapsulated {@link IValueProperty}
	 * @param index The list index
	 */
	public EMFIndexedValuePropertyDelegator(IValueProperty delegate, int index) {
		this.delegate = delegate;
		if (index < 0) {
			throw new IllegalArgumentException("The list index must not be negative!"); //$NON-NLS-1$
		}
		this.index = index;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.core.databinding.property.value.IValueProperty#getValueType()
	 */
	@Override
	public Object getValueType() {
		return delegate.getValueType();
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.core.databinding.property.value.SimpleValueProperty#doGetValue(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected Object doGetValue(Object source) {
		final Object result = delegate.getValue(source);
		final List<Object> list = (List<Object>) result;
		if (list != null && index >= list.size()) {
			return null;
		}
		return list.get(index);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.core.databinding.property.value.SimpleValueProperty#doSetValue(java.lang.Object,
	 *      java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	protected void doSetValue(Object source, Object value) {
		final Object result = delegate.getValue(source);
		final List<Object> list = (List<Object>) result;
		list.set(index, value);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.core.databinding.property.value.SimpleValueProperty#adaptListener(org.eclipse.core.databinding.property.ISimplePropertyListener)
	 */
	@Override
	public INativePropertyListener adaptListener(ISimplePropertyListener listener) {
		// TODO return suitable listener; maybe not needed as some other implementation of SimpleValueProperty return
		// null, too.
		return null;
	}
}
