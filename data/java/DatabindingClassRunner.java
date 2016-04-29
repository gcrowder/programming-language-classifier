/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.view.test.common.swt.spi;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.jface.databinding.swt.DisplayRealm;
import org.eclipse.swt.widgets.Display;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.InitializationError;

/**
 * Custom junit class runner to setup Realm.
 *
 * @author jfaltermeier
 *
 */
public class DatabindingClassRunner extends BlockJUnit4ClassRunner {

	/**
	 * @param klass
	 * @throws InitializationError
	 */
	public DatabindingClassRunner(Class<?> klass) throws InitializationError {
		super(klass);
	}

	@Override
	public void run(final RunNotifier notifier) {
		final Display display = Display.getDefault();
		Realm.runWithDefault(DisplayRealm.getRealm(display), new Runnable() {
			@Override
			public void run() {
				DatabindingClassRunner.super.run(notifier);
			}
		});
	}
}
