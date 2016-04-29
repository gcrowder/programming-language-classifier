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
package org.eclipse.emf.ecp.view.keyattributedmr.model.test;

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
import org.eclipse.emf.ecp.view.keyattribute.test.example.ExamplePackage;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyAttributeDomainModelReference;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyattributedmrFactory;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.VKeyattributedmrPackage;
import org.eclipse.emf.ecp.view.spi.keyattributedmr.model.util.KeyattributedmrValidator;
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
public class KeyAttributeDMRValidation_Test {

	private VView view;
	private VControl control;
	private VKeyAttributeDomainModelReference keyAttribute;

	private EValidator validator;
	private BasicDiagnostic chain;
	private LinkedHashMap<Object, Object> context;
	private final Boolean createChain;
	private VFeaturePathDomainModelReference key;
	private VFeaturePathDomainModelReference value;

	public KeyAttributeDMRValidation_Test(Boolean createChain) {
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
		keyAttribute = VKeyattributedmrFactory.eINSTANCE.createKeyAttributeDomainModelReference();
		keyAttribute.setKeyValue("key"); //$NON-NLS-1$
		key = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		value = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		keyAttribute.setKeyDMR(key);
		keyAttribute.setValueDMR(value);
		control.setDomainModelReference(keyAttribute);

		validator = KeyattributedmrValidator.INSTANCE;
		context = new LinkedHashMap<Object, Object>();
		if (createChain) {
			chain = new BasicDiagnostic();
		} else {
			chain = null;
		}
	}

	private boolean validate() {
		return validator.validate(keyAttribute, chain, context);
	}

	private void noContainer() {
		control.eUnset(VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private void okKeyAttribute() {
		keyAttribute.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		keyAttribute.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediate_Container());
		keyAttribute.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
	}

	private void okKey() {
		key.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getChild_Key());
		key.setDomainModelEFeature(ExamplePackage.eINSTANCE.getKeyContainer_Key());
	}

	private void okValue() {
		value.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getChild_IntermediateTarget());
		value.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getIntermediateTarget_Target());
		value.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
	}

	private DiagnosticInfo keyAttributeKey() {
		return new DiagnosticInfo(Diagnostic.ERROR, keyAttribute,
			VKeyattributedmrPackage.eINSTANCE.getKeyAttributeDomainModelReference_KeyDMR());
	}

	private DiagnosticInfo controlDMR() {
		return new DiagnosticInfo(Diagnostic.ERROR, control, VViewPackage.eINSTANCE.getControl_DomainModelReference());
	}

	private DiagnosticInfo keyAttributeFeature() {
		return new DiagnosticInfo(Diagnostic.ERROR, keyAttribute,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEFeature());
	}

	private DiagnosticInfo keyAttributePath() {
		return new DiagnosticInfo(Diagnostic.ERROR, keyAttribute,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	private DiagnosticInfo keyAttributeKeyValue() {
		return new DiagnosticInfo(Diagnostic.ERROR, keyAttribute,
			VKeyattributedmrPackage.eINSTANCE.getKeyAttributeDomainModelReference_KeyValue());
	}

	private DiagnosticInfo keyAttributeValue() {
		return new DiagnosticInfo(Diagnostic.ERROR, keyAttribute,
			VKeyattributedmrPackage.eINSTANCE.getKeyAttributeDomainModelReference_ValueDMR());
	}

	private DiagnosticInfo keyFeature() {
		return new DiagnosticInfo(Diagnostic.ERROR, key,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEFeature());
	}

	private DiagnosticInfo valuePath() {
		return new DiagnosticInfo(Diagnostic.ERROR, value,
			VViewPackage.eINSTANCE.getFeaturePathDomainModelReference_DomainModelEReferencePath());
	}

	@Test
	public void testNoKeyDMR() {
		okKeyAttribute();
		okValue();
		keyAttribute.setKeyDMR(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeKey());
		}
	}

	@Test
	public void testNoKeyDMRNoContainer() {
		noContainer();
		okKeyAttribute();
		okValue();
		keyAttribute.setKeyDMR(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeKey());
		}
	}

	@Test
	public void testNoValueDMR() {
		okKeyAttribute();
		okKey();
		keyAttribute.setValueDMR(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeValue());
		}
	}

	@Test
	public void testNoValueDMRNoContainer() {
		noContainer();
		okKeyAttribute();
		okKey();
		keyAttribute.setValueDMR(null);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeValue());
		}
	}

	@Test
	public void testNoEFeatureOnPathToKeyAttribute() {
		okKey();
		okValue();
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeFeature());
		}
	}

	@Test
	public void testNoEFeatureOnPathToKeyAttributeNoContainer() {
		noContainer();
		okKey();
		okValue();
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeFeature());
		}
	}

	@Test
	public void testBadReferenceOnPathToKeyAttribute() {
		okKeyAttribute();
		okKey();
		okValue();
		keyAttribute.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributePath());
		}
	}

	@Test
	public void testBadReferenceOnPathToKeyAttributeNoContainer() {
		noContainer();
		okKey();
		okValue();
		keyAttribute.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		keyAttribute.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		keyAttribute.setDomainModelEFeature(ExamplePackage.eINSTANCE.getContainer_Children());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributePath());
		}
	}

	@Test
	public void testBadEFeatureOnPathToKeyAttributeSingleRef() {
		okKey();
		okValue();
		keyAttribute.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		keyAttribute.setDomainModelEFeature(ExamplePackage.eINSTANCE.getIntermediateTarget_Target());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToKeyAttributeSingleRefNoContainer() {
		noContainer();
		okKey();
		okValue();
		keyAttribute.getDomainModelEReferencePath().add(ExamplePackage.eINSTANCE.getRoot_Intermediate());
		keyAttribute.setDomainModelEFeature(ExamplePackage.eINSTANCE.getIntermediateTarget_Target());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToKeyAttributeAttribute() {
		okKey();
		okValue();
		keyAttribute.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeFeature());
		}
	}

	@Test
	public void testBadEFeatureOnPathToKeyAttributeAttributeNoContainer() {
		noContainer();
		okKey();
		okValue();
		keyAttribute.setDomainModelEFeature(ExamplePackage.eINSTANCE.getTarget_Name());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeFeature());
		}
	}

	@Test
	public void testNoKeyValue() {
		okKeyAttribute();
		okValue();
		okKey();
		keyAttribute.eUnset(VKeyattributedmrPackage.eINSTANCE.getKeyAttributeDomainModelReference_KeyValue());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeKeyValue());
		}
	}

	@Test
	public void testNoKeyValueNoContainer() {
		noContainer();
		okKeyAttribute();
		okValue();
		okKey();
		keyAttribute.eUnset(VKeyattributedmrPackage.eINSTANCE.getKeyAttributeDomainModelReference_KeyValue());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeKeyValue());
		}
	}

	@Test
	public void testOK() {
		okKey();
		okKeyAttribute();
		okValue();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testOKNoContainer() {
		noContainer();
		okKey();
		okKeyAttribute();
		okValue();
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadRootEClass() {
		okKey();
		okKeyAttribute();
		okValue();
		view.setRootEClass(ExamplePackage.eINSTANCE.getIntermediate());
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributePath());
		}
	}

	@Test
	public void testBadRootEClassGoodRootEClassInContext() {
		okKey();
		okKeyAttribute();
		okValue();
		view.setRootEClass(ExamplePackage.eINSTANCE.getIntermediate());
		context.put(ViewValidator.ECLASS_KEY, ExamplePackage.eINSTANCE.getRoot());
		assertTrue(validate());
		if (createChain) {
			assertEquals(Diagnostic.OK, chain.getSeverity());
			assertChain();
		}
	}

	@Test
	public void testBadKeyDMR() {
		okKeyAttribute();
		okValue();
		okKey();
		key.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeKey(), keyFeature());
		}
	}

	@Test
	public void testBadKeyDMRNoContainer() {
		noContainer();
		okKeyAttribute();
		okValue();
		okKey();
		key.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeKey(), keyFeature());
		}
	}

	@Test
	public void testBadValueDMR() {
		okKeyAttribute();
		okValue();
		okKey();
		value.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(controlDMR(), keyAttributeValue(), valuePath());
		}
	}

	@Test
	public void testBadValueDMRNoContainer() {
		noContainer();
		okKeyAttribute();
		okValue();
		okKey();
		value.getDomainModelEReferencePath().remove(0);
		assertFalse(validate());
		if (createChain) {
			assertEquals(Diagnostic.ERROR, chain.getSeverity());
			assertChain(keyAttributeValue(), valuePath());
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

		public DiagnosticInfo(int severity, EObject object, EStructuralFeature feature) {
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
