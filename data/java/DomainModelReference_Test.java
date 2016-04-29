package org.eclipse.emf.ecp.view.model.test;

import static org.junit.Assert.assertEquals;

import java.util.LinkedList;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.junit.Before;
import org.junit.Test;

public class DomainModelReference_Test {

	@Before
	public void setUp() throws Exception {

	}

	@Test
	public void testFeaturePathDomainModelReferencePathWithNonUniqueReferences() {
		final VControl control = VViewFactory.eINSTANCE.createControl();
		final LinkedList<EReference> referencePath = new LinkedList<EReference>();
		final EReference ref1 = EcoreFactory.eINSTANCE.createEReference();
		final EReference ref2 = EcoreFactory.eINSTANCE.createEReference();
		referencePath.add(ref1);
		referencePath.add(ref2);
		referencePath.add(ref1);
		final EReference feature = EcoreFactory.eINSTANCE.createEReference();
		control.setDomainModelReference(feature, referencePath);
		assertEquals(3, referencePath.size());
		assertEquals(3, ((VFeaturePathDomainModelReference) control.getDomainModelReference())
			.getDomainModelEReferencePath().size());

	}

}
