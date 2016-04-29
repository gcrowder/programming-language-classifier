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
package org.eclipse.emf.ecp.view.edapt.test._160to170;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;

import org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest;
import org.eclipse.emf.ecp.view.edapt.test.model.ModelPackage;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;

public class DomainWithModelAsPackageNameTest extends AbstractMigrationTest {

	@Override
	protected void performTest() throws Exception {
		assertFalse(getMigrator().checkMigration(getURI()));
		getMigrator().performMigration(getURI());
		final VView view = getMigratedView();
		assertEquals(1, view.getChildren().size());
		final VControl control = VControl.class.cast(view.getChildren().get(0));
		final VFeaturePathDomainModelReference dmr = VFeaturePathDomainModelReference.class
			.cast(control.getDomainModelReference());
		assertSame(ModelPackage.eINSTANCE.getFoo_Bar(), dmr.getDomainModelEFeature());
	}

	@Override
	protected String getPath() {
		return "160/DomainWithModelName.view";
	}

}
