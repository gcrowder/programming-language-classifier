/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Edgar Mueller - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.ui.editor.test;

import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.DefaultReferenceService;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTView;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.test.common.spi.GCCollectable;
import org.eclipse.jface.databinding.swt.DisplayRealm;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swtbot.swt.finder.SWTBotTestCase;
import org.eclipse.swtbot.swt.finder.finders.UIThreadRunnable;
import org.eclipse.swtbot.swt.finder.results.Result;
import org.eclipse.swtbot.swt.finder.results.VoidResult;
import org.junit.Before;
import org.junit.Test;

/**
 * Common base class for SWTBot tests.
 *
 * @author emueller
 * @author jfaltermeier
 *
 */
public abstract class ECPCommonSWTBotTest extends SWTBotTestCase {

	private static Shell shell;
	private Display display;
	private GCCollectable swtViewCollectable;
	private EObject domainObject;

	@Override
	@Before
	public void setUp() {
		display = Display.getDefault();
		shell = UIThreadRunnable.syncExec(display, new Result<Shell>() {
			@Override
			public Shell run() {
				final Shell shell = new Shell(display);
				shell.setLayout(new FillLayout());
				return shell;
			}
		});
	}

	@Test
	public void test() throws ECPRendererException,
		InterruptedException {
		Realm.runWithDefault(DisplayRealm.getRealm(display), new TestRunnable());
	}

	public abstract EObject createDomainObject();

	public abstract VView createView();

	public abstract void logic();

	public GCCollectable getSWTViewCollectable() {
		return swtViewCollectable;
	}

	public void unsetSWTViewCollectable() {
		swtViewCollectable = null;
	}

	public void unsetDomainObject() {
		domainObject = null;
	}

	/**
	 * Can be overridden to add assertions at end of execution.
	 */
	public void assertions(double memBefore, double memAfter) {
	}

	public EObject getDomainObject() {
		return domainObject;
	}

	public void setDomainObject(EObject eObject) {
		domainObject = eObject;
	}

	private class TestRunnable implements Runnable {

		private double memBefore;
		private double memAfter;

		@Override
		public void run() {
			try {
				UIThreadRunnable.syncExec(new Result<Void>() {

					@Override
					public Void run() {
						try {
							final EObject domainObject = createDomainObject();
							memBefore = usedMemory();

							final ViewModelContext viewContext = ViewModelContextFactory.INSTANCE
								.createViewModelContext(createView(), domainObject, new DefaultReferenceService());
							final ECPSWTView swtView = ECPSWTViewRenderer.INSTANCE.render(shell, viewContext);
							swtViewCollectable = new GCCollectable(swtView);
							final Composite composite = (Composite) swtView.getSWTControl();
							final GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
							composite.setLayoutData(gridData);

							shell.open();
							return null;
						} catch (final NoRendererFoundException e) {
							fail(e.getMessage());
						} catch (final NoPropertyDescriptorFoundExeption e) {
							fail(e.getMessage());
						} catch (final ECPRendererException ex) {
							fail(ex.getMessage());
						}
						return null;
					}
				});
				logic();
			} finally {
				UIThreadRunnable.syncExec(new VoidResult() {
					@Override
					public void run() {
						shell.close();
						shell.dispose();
						memAfter = usedMemory();
					}
				});
				assertions(memBefore, memAfter);
			}
		}

	}

	public double usedMemory() {
		Runtime.getRuntime().gc();
		return 0d + Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory();
	}

}
