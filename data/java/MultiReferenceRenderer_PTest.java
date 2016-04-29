/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Lucas Koehler - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.internal.control.multireference;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.databinding.observable.IObserving;
import org.eclipse.core.databinding.observable.Observables;
import org.eclipse.core.databinding.observable.Realm;
import org.eclipse.core.databinding.observable.list.IObservableList;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.spi.util.swt.ImageRegistryService;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.D;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestFactory;
import org.eclipse.emfforms.core.services.databinding.testmodel.test.model.TestPackage;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.DatabindingFailedException;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.label.NoLabelFoundException;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;

/**
 * JUnit plugin tests for {@link MultiReferenceSWTRenderer}.
 *
 * @author Lucas Koehler
 *
 */
@RunWith(DatabindingClassRunner.class)
public class MultiReferenceRenderer_PTest {
	private static final String TEST_DESCRIPTION = "test-description"; //$NON-NLS-1$
	private static final String TEST_DISPLAYNAME = "test-displayName"; //$NON-NLS-1$
	private static Realm realm;
	private EMFFormsDatabinding databindingService;
	private MultiReferenceSWTRenderer renderer;
	private Shell shell;
	private EMFFormsLabelProvider labelProvider;

	/**
	 * Get {@link Realm} for the tests.
	 */
	@BeforeClass
	public static void setUpBeforeClass() {
		realm = Realm.getDefault();

	}

	/**
	 * Set up executed before every test.
	 * Mocks and registers the databinding and label services.
	 * Creates a new {@link MultiReferenceSWTRenderer} to be tested. Mocks needed parameters and contents (e.g.
	 * VControl, ViewModelContext).
	 *
	 * @throws DatabindingFailedException if the databinding failed
	 * @throws NoLabelFoundException
	 */
	@Before
	public void setUp() throws DatabindingFailedException, NoLabelFoundException {
		databindingService = mock(EMFFormsDatabinding.class);
		labelProvider = mock(EMFFormsLabelProvider.class);
		when(labelProvider.getDescription(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			Observables.constantObservableValue(TEST_DESCRIPTION));
		when(labelProvider.getDisplayName(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			Observables.constantObservableValue(TEST_DISPLAYNAME));

		shell = new Shell();

		final D eObject = TestFactory.eINSTANCE.createD();
		final EStructuralFeature eStructuralFeature = TestPackage.eINSTANCE.getD_YList();

		final ReportService reportService = mock(ReportService.class);
		final ViewModelContext viewContext = mock(ViewModelContext.class);
		final VControl vControl = Mockito.mock(VControl.class);
		final Setting setting = mock(Setting.class);
		final VDomainModelReference domainModelReference = mock(VDomainModelReference.class);

		when(viewContext.getDomainModel()).thenReturn(eObject);
		when(viewContext.getViewModel()).thenReturn(vControl);

		when(vControl.getDomainModelReference()).thenReturn(domainModelReference);

		when(setting.getEObject()).thenReturn(eObject);
		when(setting.getEStructuralFeature()).thenReturn(eStructuralFeature);

		final ImageRegistryService imageRegistryService = mock(ImageRegistryService.class);
		final VTViewTemplateProvider templateProvider = mock(VTViewTemplateProvider.class);

		renderer = new MultiReferenceSWTRenderer(vControl, viewContext, reportService, databindingService,
			labelProvider, templateProvider, imageRegistryService);
		renderer.init();
	}

	/**
	 * Unregister databinding and label service after every test.
	 */
	@After
	public void tearDown() {
	}

	/**
	 * Test if the initial data binding is working.
	 *
	 * @throws NoRendererFoundException Renderer could not be found
	 * @throws NoPropertyDescriptorFoundExeption Property descriptor could not be found
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testDatabindingServiceUsageInitialBinding() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final List<Integer> initialList = new LinkedList<Integer>();
		initialList.add(2);
		initialList.add(4);
		final WritableList mockedObservableList = new WritableList(realm, initialList, Integer.class);

		final Table table = setUpDatabindingTests(mockedObservableList);

		assertEquals(mockedObservableList.size(), table.getItemCount());
		for (int i = 0; i < mockedObservableList.size(); i++) {
			assertEquals(mockedObservableList.get(i).toString(), table.getItems()[i].getText(0));
		}
	}

	/**
	 * Tests whether adding values to the model is reflected in the control.
	 *
	 * @throws NoRendererFoundException Renderer could not be found
	 * @throws NoPropertyDescriptorFoundExeption Property descriptor could not be found
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testDatabindingServiceUsageAddToModel() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final List<Integer> initialList = new LinkedList<Integer>();
		initialList.add(2);
		initialList.add(4);
		final WritableList mockedObservableList = new WritableList(realm, initialList, Integer.class);

		final Table table = setUpDatabindingTests(mockedObservableList);

		mockedObservableList.add(new Integer(6));

		assertEquals(mockedObservableList.size(), table.getItemCount());
		for (int i = 0; i < mockedObservableList.size(); i++) {
			assertEquals(mockedObservableList.get(i).toString(), table.getItems()[i].getText(0));
		}
	}

	/**
	 * Tests whether removing values to the model is reflected in the control.
	 *
	 * @throws NoRendererFoundException Renderer could not be found
	 * @throws NoPropertyDescriptorFoundExeption Property descriptor could not be found
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testDatabindingServiceUsageRemoveFromModel() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final List<Integer> initialList = new LinkedList<Integer>();
		initialList.add(2);
		initialList.add(4);
		final WritableList mockedObservableList = new WritableList(realm, initialList, Integer.class);

		final Table table = setUpDatabindingTests(mockedObservableList);

		mockedObservableList.remove(0);

		assertEquals(mockedObservableList.size(), table.getItemCount());
		for (int i = 0; i < mockedObservableList.size(); i++) {
			assertEquals(mockedObservableList.get(i).toString(), table.getItems()[i].getText(0));
		}
	}

	/**
	 * Tests whether changing values of the model is reflected in the control.
	 *
	 * @throws NoRendererFoundException Renderer could not be found
	 * @throws NoPropertyDescriptorFoundExeption Property descriptor could not be found
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testDatabindingServiceUsageChangeModel() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		final List<Integer> initialList = new LinkedList<Integer>();
		initialList.add(2);
		initialList.add(4);
		final WritableList mockedObservableList = new WritableList(realm, initialList, Integer.class);

		final Table table = setUpDatabindingTests(mockedObservableList);

		mockedObservableList.set(1, 7);

		assertEquals(mockedObservableList.size(), table.getItemCount());
		for (int i = 0; i < mockedObservableList.size(); i++) {
			assertEquals(mockedObservableList.get(i).toString(), table.getItems()[i].getText(0));
		}
	}

	private Table setUpDatabindingTests(IObservableList mockedObservableList) throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, DatabindingFailedException {
		when(databindingService.getObservableList(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			mockedObservableList);
		final TestObservableValue observableValue = mock(TestObservableValue.class);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			observableValue);
		when(observableValue.getObserved()).thenReturn(mock(EObject.class));
		final Composite composite = (Composite) renderer.render(new SWTGridCell(0, 0, renderer), shell);
		final Composite controlComposite = (Composite) composite.getChildren()[1];
		final Table table = (Table) controlComposite.getChildren()[0];

		return table;
	}

	/**
	 * Tests whether a {@link EMFFormsLabelProvider} is used to get labels.
	 *
	 * @throws NoRendererFoundException Renderer could not be found
	 * @throws NoPropertyDescriptorFoundExeption Property descriptor could not be found
	 * @throws DatabindingFailedException if the databinding failed
	 */
	@Test
	public void testLabelServiceUsage() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		DatabindingFailedException {

		final TestObservableValue observableValue = mock(TestObservableValue.class);
		when(databindingService.getObservableValue(any(VDomainModelReference.class), any(EObject.class))).thenReturn(
			observableValue);
		when(observableValue.getObserved()).thenReturn(mock(EObject.class));

		final Composite composite = (Composite) renderer.render(new SWTGridCell(0, 0, renderer), shell);
		final Composite controlComposite = (Composite) composite.getChildren()[1];
		final Table table = (Table) controlComposite.getChildren()[0];

		final TableColumn column = table.getColumn(0);
		assertEquals(TEST_DISPLAYNAME, column.getText());
		assertEquals(TEST_DESCRIPTION, column.getToolTipText());
	}

	/**
	 * Helper Interface for mocking.
	 *
	 * @author Eugen Neufeld
	 *
	 */
	public interface TestObservableValue extends IObservableValue, IObserving {
	}
}
