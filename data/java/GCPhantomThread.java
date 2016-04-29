/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.test.common.spi;

import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;

class GCPhantomThread extends Thread {

	private static final int DEFAULT_TIMEOUT = 1000;

	private boolean hasReference;
	private final ReferenceQueue<?> queue;
	private final PhantomReference<?> expectedReference;
	private int retry;

	GCPhantomThread(ReferenceQueue<?> queue,
		PhantomReference<?> expectedReference) {
		this.queue = queue;
		this.expectedReference = expectedReference;
	}

	public boolean didCollectReference() {
		return hasReference;
	}

	@Override
	public void run() {
		while (!hasReference && retry < 100) {
			Reference<?> ref = null;
			System.gc();
			System.runFinalization();
			try {
				Thread.sleep(50);
			} catch (final InterruptedException e) {
				// ignore
			}
			try {
				ref = queue.remove(DEFAULT_TIMEOUT);
			} catch (final IllegalArgumentException e) {
				// ignore
			} catch (final InterruptedException e) {
				// ignore
			}
			if (ref != null && ref == expectedReference) {
				hasReference = true;
			}
			retry++;
		}
	}
}