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
package org.eclipse.emf.ecp.view.mappingdmr.model.test;

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
import org.eclipse.emf.ecp.view.mapping.test.example.ExamplePackage;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingDomainModelReference;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingdmrFactory;
import org.eclipse.emf.ecp.view.spi.mappingdmr.model.VMappingdmrPackage;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.model.util.ViewValidator;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * @author jfaltermeier
 *
 */
@RunWith(Parameterized.class)
public class MappingDMRValidation_Test {
	private VView view;
	private VControl control;
	private VMappingDomainModelReference mapping;

	private EValidator validator;
	private BasicDiagnostic chain;
	private LinkedHashMap<Object, Object> context;
	private final Boolean createChain;
	private VFeaturePathDomainModelReference target;

	public MappingDMRValidation_Test(Boolean createChain) {
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

	@SuppressWarnings("restriction")
	@Before
	public void before() {
		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(ExamplePackage.eINSTANCE.getRoot());
		control = VViewFactory.eINSTANCE.createControl();
		view.getChildren().add(control);
		mapping = VMappingdmrFactory.eINSTANCE.createMappingDomainModelReference();
		mapping.setMappedClass(ExamplePackage.eINSTANCE.getChild());
		target = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		mapping.setDomainModelReference(target);
		control.setDomainModelReference(mapping);

		validator = org.eclipse.emf.ecp.view.spi.mappingdmr.model.util.MappingdmrValidator.INSTANCE;
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

	private void okMapping() {
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
	}

	private void okTarget() {
		target.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getChild_IntermediateTarget());
		target.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediateTarget_Target());
		target.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
	}

	private DiagnosticInfo controlDMR() {
		return new DiagnosticInfo(Diagnostic.ERROR, control, VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo mappingFeature() {
		return new DiagnosticInfo(Diagnostic.ERROR, mapping,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEFeature());
	}

	private DiagnosticInfo mappingPath() {
		return new DiagnosticInfo(Diagnostic.ERROR, mapping,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	private DiagnosticInfo mappingTarget() {
		return new DiagnosticInfo(Diagnostic.ERROR, mapping,
			VMappingdmrPackage.eINSTANCE.getMappingDomainModelReference_DomainModelReference());
	}

	private DiagnosticInfo targetPath() {
		return new DiagnosticInfo(Diagnostic.ERROR, target,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	private boolean validate() {
		return validator.validate(mapping, chain, context);
	}

	@Test
	public void testNoTargetDMR() {
		okMapping();
		mapping.setDomainModelReference(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingTarget());
		}
	}

	@Test
	public void testNoTargetDMRNoContainer() {
		noContainer();
		okMapping();
		mapping.setDomainModelReference(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingTarget());
		}
	}

	@Test
	public void testNoEFeatureOnPathToIndex() {
		okTarget();
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingFeature());
		}
	}

	@Test
	public void testNoEFeatureOnPathToIndexNoContainer() {
		noContainer();
		okTarget();
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingFeature());
		}
	}

	@Test
	public void testBadReferenceOnPathToIndex() {
		okTarget();
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingPath());
		}
	}

	@Test
	public void testBadReferenceOnPathToIndexNoContainer() {
		noContainer();
		okTarget();
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingPath());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexSingleReference() {
		okTarget();
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getIntermediate_Container());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexSingleReferenceNoContainer() {
		noContainer();
		okTarget();
		mapping.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getIntermediate_Container());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexAttribute() {
		okTarget();
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToIndexAttributeNoContainer() {
		noContainer();
		okTarget();
		mapping.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingFeature());
		}
	}

	@Test
	public void testBadClass() {
		okMapping();
		okTarget();
		mapping.setMappedClass(ExamplePackage.eINSTANCE.getContainer());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingTarget(), targetPath());
		}
	}

	@Test
	public void testBadClassNoContainer() {
		noContainer();
		okMapping();
		okTarget();
		mapping.setMappedClass(ExamplePackage.eINSTANCE.getContainer());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingTarget(), targetPath());
		}
	}

	@Test
	public void testOK() {
		okMapping();
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
		okMapping();
		okTarget();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadRootEClass() {
		okMapping();
		okTarget();
		view.setRootEClass(ExamplePackage.eINSTANCE.getIntermediate());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingPath());
		}
	}

	@Test
	public void testBadRootEClassGoodRootEClassInContext() {
		okMapping();
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
		okMapping();
		okTarget();
		target.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), mappingTarget(), targetPath());
		}
	}

	@Test
	public void testBadPathOnTargetNoContainer() {
		noContainer();
		okMapping();
		okTarget();
		target.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(mappingTarget(), targetPath());
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
