/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 * 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.custom.ui.swt.test;

import static org.junit.Assert.assertEquals;

import org.eclipse.emf.common.util.Diagnostic;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomControl;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomDomainModelReference;
import org.eclipse.emf.ecp.view.spi.custom.model.VCustomFactory;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.Player;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * @author Eugen Neufeld
 * 
 */
@RunWith(DatabindingClassRunner.class)
public class CustomControlValidation_PTest {

	private VCustomControl modelControl;
	private VView view;

	@Before
	public void init() {
		view = VViewFactory.eINSTANCE.createView();
		modelControl = VCustomFactory.eINSTANCE.createCustomControl();
		final VCustomDomainModelReference domainModelReference = VCustomFactory.eINSTANCE
			.createCustomDomainModelReference();
		modelControl.setDomainModelReference(domainModelReference);
		// domainModelReference.setControlId("org.eclipse.emf.ecp.view.custom.ui.swt.test.ValidatiuonCustomControl");
		modelControl.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		modelControl
			.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.ValidationCustomControl");

		domainModelReference.setBundleName("org.eclipse.emf.ecp.view.custom.ui.swt.test");
		domainModelReference
			.setClassName("org.eclipse.emf.ecp.view.custom.ui.swt.test.ValidationCustomControl");

		view.getChildren().add(modelControl);
	}

	@Test
	public void testValidateOnInitValid() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		player.getEMails().add("bla");

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);

		assertEquals("Severity must be ok", Diagnostic.OK, modelControl.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidateOnInitInvalid() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		assertEquals("Severity must be error", Diagnostic.ERROR, modelControl.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidateChangeValidToInvalid() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();
		player.getEMails().add("bla");

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		player.getEMails().clear();
		assertEquals("Severity must be error", Diagnostic.ERROR, modelControl.getDiagnostic().getHighestSeverity());
	}

	@Test
	public void testValidateChangeInvalidToValid() {
		final Player player = BowlingFactory.eINSTANCE.createPlayer();

		ViewModelContextFactory.INSTANCE.createViewModelContext(view, player);
		player.getEMails().add("bla");
		assertEquals("Severity must be ok", Diagnostic.OK, modelControl.getDiagnostic().getHighestSeverity());

	}

}
