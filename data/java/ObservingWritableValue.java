/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.core.swt.tests;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.core.databinding.observable.IObserving;
import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.value.WritableValue;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.InternalEObject;

/**
 * A {@link WritableValue} that returns a mocked EObject for {@link IObserving#getObserved()}.
 *
 * @author Lucas Koehler
 *
 */
public class ObservingWritableValue extends WritableValue implements IObserving {

	/**
	 * Creates a new instance.
	 *
	 * @param realm The realm.
	 * @param initialValue The initial value
	 * @param valueType The value type
	 */
	public ObservingWritableValue(Realm realm, Object initialValue, Object valueType)
	{
		super(realm, initialValue, valueType);
	}

	/**
	 * Creates a new instance based on another ObservingWritableValue.
	 *
	 * @param observingWritableValue The ObservingWritableValue.
	 */
	public ObservingWritableValue(ObservingWritableValue observingWritableValue)
	{
		super(observingWritableValue.getRealm(), observingWritableValue.getValue(), observingWritableValue
			.getValueType());
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.core.databinding.observable.IObserving#getObserved()
	 */
	@Override
	public Object getObserved() {
		final InternalEObject eObject = mock(InternalEObject.class);
		final Setting setting = mock(Setting.class);
		when(eObject.eSetting(any(EStructuralFeature.class))).thenReturn(setting);
		return eObject;
	}
}
