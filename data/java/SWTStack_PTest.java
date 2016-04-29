/*******************************************************************************
 * Copyright (c) 2011-2014 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.stack.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTView;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackFactory;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackItem;
import org.eclipse.emf.ecp.view.spi.stack.model.VStackLayout;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.BowlingPackage;
import org.eclipse.emf.emfstore.bowling.Gender;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(DatabindingClassRunner.class)
public class SWTStack_PTest {

	private Player domain;
	private VView view;
	private VStackLayout stackLayout;
	private VStackItem femaleItem;
	private VStackItem maleItem;
	private Shell shell;

	@Before
	public void before() {
		domain = BowlingFactory.eINSTANCE.createPlayer();

		view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(BowlingPackage.eINSTANCE.getPlayer());

		stackLayout = VStackFactory.eINSTANCE.createStackLayout();
		view.getChildren().add(stackLayout);

		femaleItem = TestUtil.createItem(Gender.FEMALE, BowlingPackage.eINSTANCE.getPlayer_Name());
		stackLayout.getStackItems().add(femaleItem);

		maleItem = TestUtil.createItem(Gender.MALE, BowlingPackage.eINSTANCE.getPlayer_Gender());
		stackLayout.getStackItems().add(maleItem);
	}

	@After
	public void after() {
		shell.dispose();
	}

	private Composite render() throws ECPRendererException {
		shell = new Shell();
		final ECPSWTView render = ECPSWTViewRenderer.INSTANCE.render(shell, domain, view);
		final Composite swtControl = (Composite) render.getSWTControl();
		return (Composite) swtControl.getChildren()[0];
	}

	private Composite getTop(Composite stack) {
		final StackLayout layout = (StackLayout) stack.getLayout();
		return (Composite) layout.topControl;
	}

	private void assertContainsText(Composite composite) {
		composite = (Composite) composite.getChildren()[0];
		boolean textFound = false;
		for (final Control control : composite.getChildren()) {
			if (control instanceof org.eclipse.swt.widgets.Text) {
				textFound = true;
				break;
			}
			if (control instanceof Composite) {
				final Control textComposite = Composite.class.cast(control).getChildren()[0];
				if (textComposite instanceof org.eclipse.swt.widgets.Text) {
					textFound = true;
					break;
				}
			}
		}
		if (textFound) {
			return;
		}
		fail("Composite does not contain a Text");
	}

	private void assertContainsCombo(Composite composite) {
		composite = (Composite) composite.getChildren()[0];
		boolean comboFound = false;
		for (final Control control : composite.getChildren()) {
			if (control instanceof Combo) {
				comboFound = true;
				break;
			}
		}
		if (comboFound) {
			return;
		}
		fail("Composite does not contain a Combo");
	}

	@Test
	public void testInitMale() throws ECPRendererException {
		stackLayout.setTopElement(maleItem);
		final Composite render = render();
		assertContainsCombo(getTop(render));
	}

	@Test
	public void testInitFemale() throws ECPRendererException {
		stackLayout.setTopElement(femaleItem);
		final Composite render = render();
		assertContainsText(getTop(render));
	}

	@Test
	public void testDynamic() throws ECPRendererException {
		final Composite render = render();
		stackLayout.setTopElement(maleItem);
		assertContainsCombo(getTop(render));
		stackLayout.setTopElement(femaleItem);
		assertContainsText(getTop(render));
	}

	@Test
	public void testSetTopToNull() throws ECPRendererException {
		stackLayout.getStackItems().remove(maleItem);
		stackLayout.setTopElement(femaleItem);
		final Composite render = render();
		assertContainsText(getTop(render));
		stackLayout.setTopElement(null);
		final Composite top = getTop(render);
		assertEquals(0, top.getChildren().length);
	}
}
