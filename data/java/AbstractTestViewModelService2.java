/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emfforms.internal.core.services.legacy;

import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;

public abstract class AbstractTestViewModelService2 implements ITestViewModelService2 {

	@Override
	public void instantiate(ViewModelContext context) {
		// used for verification
		context.putContextValue(getClass().getSimpleName(), getClass());
	}

	@Override
	public void dispose() {
		// no op
	}
}
