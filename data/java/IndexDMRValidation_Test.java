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
package org.eclipse.emf.ecp.view.indexdmr.model.test;

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

import org.eclipse.emf.common.util.BasicDiagnostic;
import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EValidator;
import org.eclipse.emf.ecp.view.index.test.example.ExamplePackage;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexDomainModelReference;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrFactory;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.VIndexdmrPackage;
import org.eclipse.emf.ecp.view.spi.indexdmr.model.util.IndexdmrValidator;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.model.util.ViewValidator;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Errors on path to index
 * Index negative
 * Errors on target Path
 *
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class IndexDMRValidation_Test {

	private VView view;
	private VControl control;
	private VIndexDomainModelReference index;

	private EValidator validator;
	private BasicDiagnostic chain;
	private LinkedHashMap<Object, Object> context;
	private final Boolean createChain;
	private VFeaturePathDomainModelReference target;

	public IndexDMRValidation_Test(Boolean createChain) {
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
		view.setRootEClass(ExamplePackage.eINSTANCE.getRoot());
		control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		index = VIndexdmrFactory.eINSTANCE.createIndexDomainModelReference();
		target = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		index.setTargetDMR(target);
		index.setPrefixDMR(VViewFactory.eINSTANCE.createFeaturePathDomainModelReference());
		control.setDomainModelReference(index);

		validator = IndexdmrValidator.INSTANCE;
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

	private void okTarget() {
		target.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getChild_IntermediateTarget());
		target.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediateTarget_Target());
		target.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
	}

	private void okIndex() {
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
	}

	private DiagnosticInfo controlDMR() {
		return new DiagnosticInfo(Diagnostic.ERROR, control, VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo indexFeature() {
		return new DiagnosticInfo(Diagnostic.ERROR, index,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEFeature());
	}

	private DiagnosticInfo indexPath() {
		return new DiagnosticInfo(Diagnostic.ERROR, index,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	private DiagnosticInfo indexIndex() {
		return new DiagnosticInfo(Diagnostic.ERROR, index,
			VIndexdmrPackage.eINSTANCE.getIndexDomainModelReference_Index());
	}

	private DiagnosticInfo indexTarget() {
		return new DiagnosticInfo(Diagnostic.ERROR, index,
			VIndexdmrPackage.eINSTANCE.getIndexDomainModelReference_TargetDMR());
	}

	private DiagnosticInfo targetPath() {
		return new DiagnosticInfo(Diagnostic.ERROR, target,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	private boolean validate() {
		return validator.validate(index, chain, context);
	}

	@Test
	public void testNoTargetDMR() {
		okIndex();
		index.setTargetDMR(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexTarget());
		}
	}

	@Test
	public void testNoTargetDMRNoContainer() {
		noContainer();
		okIndex();
		index.setTargetDMR(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexTarget());
		}
	}

	// FIXME review test case
	@Ignore
	@Test
	public void testNoEFeatureOnPathToIndex() {
		okTarget();
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexFeature());
		}
	}

	/**
	 * Re-enable after FIXMEs in IndexdmrValidator are addressed
	 */
	@Ignore
	@Test
	public void testNoEFeatureOnPathToIndexNoContainer() {
		noContainer();
		okTarget();
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexFeature());
		}
	}

	@Test
	public void testBadReferenceOnPathToIndex() {
		okTarget();
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexPath());
		}
	}

	@Test
	public void testBadReferenceOnPathToIndexNoContainer() {
		noContainer();
		okTarget();
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexPath());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexSingleReference() {
		okTarget();
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getIntermediate_Container());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexSingleReferenceNoContainer() {
		noContainer();
		okTarget();
		index.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getIntermediate_Container());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexAttribute() {
		okTarget();
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexAttributeNoContainer() {
		noContainer();
		okTarget();
		index.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexFeature());
		}
	}

	@Test
	public void testBadIndex() {
		okIndex();
		okTarget();
		index.setIndex(-1);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexIndex());
		}
	}

	@Test
	public void testBadIndexNoContainer() {
		noContainer();
		okIndex();
		okTarget();
		index.setIndex(-1);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexIndex());
		}
	}

	@Test
	public void testOK() {
		okIndex();
		okTarget();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testOKNoContainer() {
		noContainer();
		okIndex();
		okTarget();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadRootEClass() {
		okIndex();
		okTarget();
		view.setRootEClass(ExamplePackage.eINSTANCE.getIntermediate());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexPath());
		}
	}

	@Test
	public void testBadRootEClassGoodRootEClassInContext() {
		okIndex();
		okTarget();
		view.setRootEClass(ExamplePackage.eINSTANCE.getIntermediate());
		context.put(ViewValidator.ECLASS_KEY, ExamplePackage.eINSTANCE.getRoot());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadPathOnTarget() {
		okIndex();
		okTarget();
		target.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), indexTarget(), targetPath());
		}
	}

	@Test
	public void testBadPathOnTargetNoContainer() {
		noContainer();
		okIndex();
		okTarget();
		target.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(indexTarget(), targetPath());
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
