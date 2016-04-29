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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest;
import org.eclipse.emf.ecp.view.spi.model.LabelAlignment;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VView;

public class LabelAlignmentTest extends AbstractMigrationTest {

	@Override
	// BEGIN SUPRESS CATCH EXCEPTION
	protected void performTest() throws Exception {// END SUPRESS CATCH EXCEPTION
		assertFalse(getMigrator().checkMigration(getURI()));
		getMigrator().performMigration(getURI());
		final VView view = getMigratedView();
		assertEquals(2, view.getChildren().size());
		assertEquals(LabelAlignment.LEFT, VControl.class.cast(view.getChildren().get(0)).getLabelAlignment());
		assertEquals(LabelAlignment.NONE, VControl.class.cast(view.getChildren().get(1)).getLabelAlignment());
	}

	@Override
	protected String getPath() {
		return "160/LabelAlignment.view";
	}

}
