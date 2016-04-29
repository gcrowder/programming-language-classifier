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
package org.eclipse.emf.ecp.view.edapt.test._140to170;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.ecore.xmi.XMIResource;
import org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;

/**
 * Checks whether uuids are preserved during migration
 *
 * @author jfaltermeier
 *
 */
public class EmptyTableWithUUID_PTest extends AbstractMigrationTest {

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest#performTest()
	 */
	@Override
	// BEGIN SUPRESS CATCH EXCEPTION
	protected void performTest() throws Exception {// END SUPRESS CATCH EXCEPTION
		assertFalse(getMigrator().checkMigration(getURI()));
		getMigrator().performMigration(getURI());
		final VView view = getMigratedView();
		assertEquals(1, view.getChildren().size());
		assertTrue(VTableControl.class.isInstance(view.getChildren().get(0)));
		final VTableControl tableControl = VTableControl.class.cast(view.getChildren().get(0));
		assertEquals("_bfW9gBsWEeWtk5zWlGll4Q", XMIResource.class.cast(view.eResource()).getID(view));
		assertEquals("_bfW9gRsWEeWtk5zWlGll4Q", XMIResource.class.cast(view.eResource()).getID(tableControl));
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest#getPath()
	 */
	@Override
	protected String getPath() {
		return "140/EmptyTableWithUUID.view";
	}

}
