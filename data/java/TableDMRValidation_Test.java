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
package org.eclipse.emf.ecp.view.spi.table.model.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.databinding.property.Properties;
import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.model.util.ViewValidator;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.spi.table.model.VTablePackage;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.mockito.Mockito;

/**
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class TableDMRValidation_Test {
	private VView view;
	private VControl control;
	private VTableDomainModelReference table;

	private TableValidator validator;
	private BasicDiagnostic chain;
	private LinkedHashMap<Object, Object> context;
	private final Boolean createChain;
	private VFeaturePathDomainModelReference column1;
	private VFeaturePathDomainModelReference column2;
	private VFeaturePathDomainModelReference tableDMR;
	private EMFFormsDatabinding emfFormsDatabinding;

	public TableDMRValidation_Test(Boolean createChain) {
		this.createChain = createChain;
	}

	@Parameters
	public static Collection<Object[]> data() {
		// run all tests once with a diagnostic chain and once without
		final List<Object[]> parameters = new ArrayList<Object[]>();
		parameters.add(new Object[] { true });
		parameters.add(new Object[] { false });
		return parameters;
	}

	@Before
	public void before() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getReferee());
		control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		table = VTableFactory.eINSTANCE.createTableDomainModelReference();
		tableDMR = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		table.setDomainModelReference(tableDMR);
		column1 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		column2 = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		table.getColumnDomainModelReferences().add(column1);
		table.getColumnDomainModelReferences().add(column2);
		control.setDomainModelReference(table);

		emfFormsDatabinding = Mockito.mock(EMFFormsDatabinding.class);
		validator = new TableValidator(emfFormsDatabinding);
		context = new LinkedHashMap<Object, Object>();
		if (createChain) {
			chain = new BasicDiagnostic();
		} else {
			chain = null;
		}
	}

	private void noContainer() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private void okTable() {
		final VFeaturePathDomainModelReference ref = (VFeaturePathDomainModelReference) table.getDomainModelReference();
		ref.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getReferee_League());
		ref.setDomainModelEFeature(BowlingPackage.eINSTANCE.getLeague_Players());
		table.setDomainModelReference(ref);
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getLeague_Players())).when(
				emfFormsDatabinding).getValueProperty(ref, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
	}

	private void okColumn1() {
		column1.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Name());
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getPlayer_Name())).when(
				emfFormsDatabinding).getValueProperty(column1, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
	}

	private void okColumn2() {
		column2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getPlayer_Gender());
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getPlayer_Gender())).when(
				emfFormsDatabinding).getValueProperty(column2, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
	}

	private boolean validate() {
		return validator.validate(table, chain, context);
	}

	private DiagnosticInfo controlDMR() {
		return new DiagnosticInfo(Diagnostic.ERROR, control, VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo controlDMRWarning() {
		return new DiagnosticInfo(Diagnostic.WARNING, control,
			VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo tableDMR() {
		return new DiagnosticInfo(Diagnostic.ERROR, table,
			VTablePackage.eINSTANCE.getTableDomainModelReference_DomainModelReference());
	}

	private DiagnosticInfo tableDMREFeature() {
		return new DiagnosticInfo(Diagnostic.ERROR, table.getDomainModelReference(),
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEFeature());
	}

	private DiagnosticInfo tableDMRPath() {
		return new DiagnosticInfo(Diagnostic.ERROR, table.getDomainModelReference(),
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	private DiagnosticInfo tableColumns() {
		return new DiagnosticInfo(Diagnostic.WARNING, table,
			VTablePackage.eINSTANCE.getTableDomainModelReference_ColumnDomainModelReferences());
	}

	@Test
	public void testNoColumns() {
		table.getColumnDomainModelReferences().clear();
		okTable();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testNoColumnsNoContainer() {
		noContainer();
		table.getColumnDomainModelReferences().clear();
		okTable();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testNoEFeatureOnPathToTable() {
		okTable();
		okColumn1();
		okColumn2();
		tableDMR.setDomainModelEFeature(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), tableDMR(), tableDMREFeature());
		}
	}

	@Test
	public void testNoEFeatureOnPathToTableNoContainer() {
		noContainer();
		okTable();
		okColumn1();
		okColumn2();
		tableDMR.setDomainModelEFeature(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(tableDMR(), tableDMREFeature());
		}
	}

	@Test
	public void testBadReferenceOnPathToTable() {
		okTable();
		okColumn1();
		okColumn2();
		tableDMR.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FanMerchandise());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), tableDMR(), tableDMRPath());
		}
	}

	@Test
	public void testBadReferenceOnPathToTableNoContainer() {
		noContainer();
		okTable();
		okColumn1();
		okColumn2();
		tableDMR.getDomainModelEReferencePath().add(BowlingPackage.eINSTANCE.getFan_FanMerchandise());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(tableDMR(), tableDMRPath());
		}
	}

	@Test
	public void testBadEFeatureSingleRef() {
		okColumn1();
		okColumn2();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		tableDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getFan_FavouritePlayer())).when(
				emfFormsDatabinding).getValueProperty(tableDMR, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), tableDMR());
		}
	}

	@Test
	public void testBadEFeatureSingleRefNoContainer() {
		noContainer();
		okColumn1();
		okColumn2();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		tableDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_FavouritePlayer());
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getFan_FavouritePlayer())).when(
				emfFormsDatabinding).getValueProperty(tableDMR, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(tableDMR());
		}
	}

	@Test
	public void testBadEFeatureAttribute() {
		okColumn1();
		okColumn2();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		tableDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_EMails());
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getFan_EMails())).when(
				emfFormsDatabinding).getValueProperty(tableDMR, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), tableDMR());
		}
	}

	@Test
	public void testBadEFeatureAttributeNoContainer() {
		noContainer();
		okColumn1();
		okColumn2();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		tableDMR.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_EMails());
		try {
			Mockito.doReturn(Properties.selfValue(BowlingPackage.eINSTANCE.getFan_EMails())).when(
				emfFormsDatabinding).getValueProperty(tableDMR, null);
		} catch (final DatabindingFailedException ex) {
			fail(ex.getMessage());
		}
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(tableDMR());
		}
	}

	@Test
	public void testBadColumn() {
		okTable();
		okColumn1();
		column2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Gender());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.WARNING, chain.getSeverity());
			assertChain(controlDMRWarning(), tableColumns());
		}
	}

	@Test
	public void testBadColumnNoContainer() {
		noContainer();
		okTable();
		okColumn1();
		column2.setDomainModelEFeature(BowlingPackage.eINSTANCE.getFan_Gender());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.WARNING, chain.getSeverity());
			assertChain(tableColumns());
		}
	}

	@Test
	public void testOK() {
		okColumn1();
		okColumn2();
		okTable();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testOKNoContainer() {
		noContainer();
		okColumn1();
		okColumn2();
		okTable();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadRootEClass() {
		okColumn1();
		okColumn2();
		okTable();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), tableDMR(), tableDMRPath());
		}
	}

	@Test
	public void testBadRootEClassGoodRootEClassInContext() {
		okColumn1();
		okColumn2();
		okTable();
		view.setRootEClass(BowlingPackage.eINSTANCE.getFan());
		context.put(ViewValidator.ECLASS_KEY, BowlingPackage.eINSTANCE.getReferee());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	private void assertChain(DiagnosticInfo... infos) {
		final Set<DiagnosticInfo> infoSet = new LinkedHashSet<DiagnosticInfo>(Arrays.asList(infos));
		assertEquals(infos.length, chain.getChildren().size());
		for (final Diagnostic child : chain.getChildren()) {
			boolean found = false;
			final Iterator<DiagnosticInfo> iterator = infoSet.iterator();
			while (iterator.hasNext()) {
				final DiagnosticInfo next = iterator.next();
				if (next.sameData(child)) {
					found = true;
					iterator.remove();
					break;
				}
			}
			if (!found) {
				fail("Chain is missing child diagnostic."); //$NON-NLS-1$
			}
		}
	}

	private class DiagnosticInfo {
		private final int severity;
		private final EObject object;
		private final EStructuralFeature feature;

		DiagnosticInfo(int severity, EObject object, EStructuralFeature feature) {
			this.severity = severity;
			this.object = object;
			this.feature = feature;
		}

		public boolean sameData(Diagnostic diagnostic) {
			if (diagnostic.getSeverity() != severity) {
				return false;
			}
			if (!object.equals(diagnostic.getData().get(0))) {
				return false;
			}
			if (!feature.equals(diagnostic.getData().get(1))) {
				return false;
			}
			return true;
		}

	}
}
