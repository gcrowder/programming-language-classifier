/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;

/**
 * @author jfaltermeier
 *
 */
public class Table_PTest extends AbstractMigrationTest {

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
		assertNotNull(tableControl.getDomainModelReference());
		assertTrue(VTableDomainModelReference.class.isInstance(tableControl.getDomainModelReference()));
		final VTableDomainModelReference dmr = VTableDomainModelReference.class.cast(tableControl
			.getDomainModelReference());
		assertNull(dmr.getDomainModelEFeature());
		assertEquals(0, dmr.getDomainModelEReferencePath().size());
		assertNotNull(dmr.getDomainModelReference());
		assertTrue(VFeaturePathDomainModelReference.class.isInstance(dmr.getDomainModelReference()));
		final VFeaturePathDomainModelReference childDMR = VFeaturePathDomainModelReference.class.cast(dmr
			.getDomainModelReference());
		assertSame(BowlingPackage.eINSTANCE.getLeague_Players(), childDMR.getDomainModelEFeature());
		assertEquals(1, childDMR.getDomainModelEReferencePath().size());
		assertSame(BowlingPackage.eINSTANCE.getReferee_League(), childDMR.getDomainModelEReferencePath().get(0));
		assertUUIDPresent(view);
		final TreeIterator<EObject> contents = view.eAllContents();
		while (contents.hasNext()) {
			final EObject next = contents.next();
			assertUUIDPresent(next);
		}
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.eclipse.emf.ecp.view.edapt.test.AbstractMigrationTest#getPath()
	 */
	@Override
	protected String getPath() {
		return "140/Table.view";
	}
}
