/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.rule.test;

import java.util.List;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.util.EContentAdapter;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeAddRemoveListener;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeNotification;

/**
 * @author Edgar
 *
 */
public class TestDomainContentAdapter extends EContentAdapter {

	private final List<ModelChangeAddRemoveListener> domainChangeListeners;

	public TestDomainContentAdapter(List<ModelChangeAddRemoveListener> listeners) {
		domainChangeListeners = listeners;
	}

	@Override
	public void notifyChanged(Notification notification) {

		super.notifyChanged(notification);

		if (notification.isTouch()) {
			return;
		}

		for (final ModelChangeAddRemoveListener listener : domainChangeListeners) {
			listener.notifyChange(new ModelChangeNotification(notification));
		}
	}
}
