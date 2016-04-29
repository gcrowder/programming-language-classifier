/**
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Jonas Helming - initial API and implementation
 */
package org.eclipse.emf.ecp.view.test.common.swt.spi;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

public final class SWTViewTestHelper {
	// private static SWTRendererFactory factory = new SWTRendererFactoryImpl();
	private static EMFFormsRendererFactory factory;

	static {
		final BundleContext bundleContext = FrameworkUtil.getBundle(SWTViewTestHelper.class).getBundleContext();
		final ServiceReference<EMFFormsRendererFactory> serviceReference = bundleContext
			.getServiceReference(EMFFormsRendererFactory.class);
		factory = bundleContext.getService(serviceReference);
	}

	private SWTViewTestHelper() {

	}

	/**
	 *
	 * @return a new {@link Shell} with a {@link FillLayout}
	 */
	public static Shell createShell() {
		final Display display = Display.getDefault();
		final Shell shell = new Shell(display);
		shell.setLayout(new FillLayout());
		return shell;
	}

	/**
	 * Renders the given {@link VElement} on the given {@link Shell} and uses the given {@link EObject} as an input.
	 *
	 * @param renderable the {@link VElement} to be rendered
	 * @param input The input {@link EObject} (domain model instance)
	 * @param shell The {@link Shell} to render on
	 * @return the rendered {@link Control}
	 * @throws NoRendererFoundException If a required sub renderer is not found
	 * @throws NoPropertyDescriptorFoundExeption If no PropertyDescriptor was found for the domain model instance
	 * @throws EMFFormsNoRendererException If the renderer for the given {@link VElement} is not found
	 */
	public static Control render(VElement renderable, EObject input, Shell shell) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		final ViewModelContext viewContext = ViewModelContextFactory.INSTANCE.createViewModelContext(renderable, input);
		final AbstractSWTRenderer<VElement> renderer = factory
			.getRendererInstance(renderable, viewContext);
		final SWTGridDescription gridDescription = renderer.getGridDescription(GridDescriptionFactory.INSTANCE
			.createEmptyGridDescription());
		final Control control = renderer.render(gridDescription.getGrid().get(gridDescription.getColumns() - 1), shell);
		renderer.finalizeRendering(shell);
		// TODO return resultRows
		if (control == null) {
			return null;
		}

		return control;

	}

	/**
	 * Renders the given {@link VElement} on the given {@link Shell}. The method will create a dummy domain model object
	 * as an input.
	 *
	 * @param renderable the {@link VElement} to be rendered
	 * @param shell The {@link Shell} to render on
	 * @return the rendered {@link Control}
	 * @throws NoRendererFoundException If a required sub renderer is not found
	 * @throws NoPropertyDescriptorFoundExeption If no PropertyDescriptor was found for the domain model instance
	 * @throws EMFFormsNoRendererException If the renderer for the given {@link VElement} is not found
	 */
	public static Control render(VElement renderable, Shell shell) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		return render(renderable, VViewFactory.eINSTANCE.createView(), shell);
	}

	public static int getNumberofColumns(Composite composite) {
		final GridLayout gridLayout = (GridLayout) composite.getLayout();
		return gridLayout.numColumns;
	}

	public static int getHorizontalSpan(Composite composite) {
		final GridData gridData = (GridData) composite.getLayoutData();
		return gridData.horizontalSpan;
	}

	public static boolean checkIfThereIsATextControlWithLabel(Control control) {
		if (!(control instanceof Composite)) {
			return false;
		}
		final Composite controlComposite = (Composite) control;

		return checkIfThereIsATextControl(controlComposite.getChildren()[2]);
	}

	public static boolean checkIfThereIsATextControl(Control control) {
		if (Text.class.isInstance(control)) {
			return true;
		} else if (Composite.class.isInstance(control)) {
			return checkIfThereIsATextControl(Composite.class.cast(control).getChildren()[0]);
		}
		return false;
	}

	public static List<Text> getAllTextControls(Control control) {
		final Composite controlComposite = (Composite) control;
		final List<Text> textFields = new ArrayList<Text>();
		for (final Control textControl : controlComposite.getChildren()) {
			if (textControl instanceof Text) {
				textFields.add((Text) textControl);
			} else if (textControl instanceof Composite) {
				textFields.addAll(getAllTextControls(textControl));
			}
		}
		return textFields;
	}
}
