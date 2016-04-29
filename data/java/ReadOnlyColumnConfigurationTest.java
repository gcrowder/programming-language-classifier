/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * jfaltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.edapt.test._160to170;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;

import org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.table.model.VReadOnlyColumnConfiguration;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;

public class ReadOnlyColumnConfigurationTest extends AbstractMigrationTest {

	@Override
	// BEGIN SUPRESS CATCH EXCEPTION
	protected void performTest() throws Exception {// END SUPRESS CATCH EXCEPTION
		assertFalse(getMigrator().checkMigration(getURI()));
		getMigrator().performMigration(getURI());
		final VView view = getMigratedView();
		final VTableControl tableControl = VTableControl.class.cast(view.getChildren().get(0));
		final VReadOnlyColumnConfiguration columnConfiguration = VReadOnlyColumnConfiguration.class
			.cast(tableControl.getColumnConfigurations().get(0));
		final VDomainModelReference reference = columnConfiguration.getColumnDomainReferences().get(0);
		final VDomainModelReference expectedReference = VTableDomainModelReference.class
			.cast(tableControl.getDomainModelReference()).getColumnDomainModelReferences()
			.get(0);
		assertSame(expectedReference, reference);
	}

	@Override
	protected String getPath() {
		return "160/League.view";
	}

}
