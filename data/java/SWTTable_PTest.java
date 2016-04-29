/*******************************************************************************
 * Copyright (c) 2011-2016 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier
 *
 *******************************************************************************/
package org.eclipse.emf.ecp.view.table.ui.swt.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Set;

import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EStructuralFeature.Setting;
import org.eclipse.emf.ecore.EcoreFactory;
import org.eclipse.emf.ecore.EcorePackage;
import org.eclipse.emf.ecp.common.spi.UniqueSetting;
import org.eclipse.emf.ecp.edit.spi.swt.table.ECPCellEditorComparator;
import org.eclipse.emf.ecp.edit.spi.swt.table.StringCellEditor;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener;
import org.eclipse.emf.ecp.view.spi.context.ViewModelService;
import org.eclipse.emf.ecp.view.spi.model.ModelChangeListener;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.model.VViewPackage;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emf.ecp.view.spi.table.model.DetailEditing;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.spi.table.swt.TableControlSWTRenderer;
import org.eclipse.emf.ecp.view.spi.util.swt.ImageRegistryService;
import org.eclipse.emf.ecp.view.template.model.VTViewTemplateProvider;
import org.eclipse.emf.ecp.view.test.common.swt.spi.DatabindingClassRunner;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTTestUtil;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.emf.EMFFormsDatabindingEMF;
import org.eclipse.emfforms.spi.core.services.editsupport.EMFFormsEditSupport;
import org.eclipse.emfforms.spi.core.services.label.EMFFormsLabelProvider;
import org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener;
import org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.di.EMFFormsContextProvider;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;

@RunWith(DatabindingClassRunner.class)
public class SWTTable_PTest {
	private static String log;
	private static ServiceReference<EMFFormsRendererFactory> factoryServiceReference;
	private static EMFFormsRendererFactory rendererFactory;
	private static PrintStream systemErr;
	private Shell shell;
	private EObject domainElement;

	@BeforeClass
	public static void beforeClass() {
		systemErr = System.err;
		System.setErr(new PrintStreamWrapper(systemErr));
		final BundleContext bundleContext = FrameworkUtil.getBundle(SWTTable_PTest.class).getBundleContext();
		factoryServiceReference = bundleContext.getServiceReference(EMFFormsRendererFactory.class);
		rendererFactory = bundleContext.getService(factoryServiceReference);
	}

	@AfterClass
	public static void afterClass() {
		System.setErr(systemErr);
		final BundleContext bundleContext = FrameworkUtil.getBundle(SWTTable_PTest.class).getBundleContext();
		bundleContext.ungetService(factoryServiceReference);
	}

	@Before
	public void init() {
		log = "";
		shell = SWTViewTestHelper.createShell();

		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		eClass.getESuperTypes().add(EcorePackage.eINSTANCE.getEClass());
		domainElement = eClass;
	}

	@After
	public void after() {
		if (!log.isEmpty()) {
			fail("Unexpected log to System.err: " + log);
		}
	}

	@Test
	public void testUninitializedTableWithoutColumns() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final TableControlHandle handle = createUninitializedTableWithoutColumns();
		//
		final Control render = SWTViewTestHelper.render(handle.getTableControl(), domainElement, shell);
		assertTrue(Label.class.isInstance(render));// Error label with error text
		assertEquals("The field domainModelEFeature of the given VFeaturePathDomainModelReference must not be null.",
			Label.class.cast(render).getText());
	}

	@Test
	public void testInitializedTableWithoutColumnsAndEmptyReference() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final EClass createEClass = EcoreFactory.eINSTANCE.createEClass();
		createEClass.eUnset(EcorePackage.eINSTANCE.getEClass_ESuperTypes());
		domainElement = createEClass;
		final TableControlHandle handle = createInitializedTableWithoutTableColumns();

		try {
			SWTViewTestHelper.render(handle.getTableControl(), domainElement, shell);
		} catch (final NullPointerException e) {
			fail("Fails without a reference in domain object");
		}

	}

	@Ignore
	@Test
	public void testInitializedTableWithoutColumnsSingleReference() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(VViewPackage.eINSTANCE.getView());
		domainElement = view;
		final TableControlHandle handle = createInitializedTableWithoutTableColumns();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(VViewPackage.eINSTANCE.getView_RootEClass());
		handle.getTableControl().setDomainModelReference(domainModelReference);

		try {
			SWTViewTestHelper.render(handle.getTableControl(), domainElement, shell);
		} catch (final ClassCastException e) {
			e.printStackTrace();
			fail("Fails with single reference in domain object");
		}

	}

	@Ignore
	@Test
	public void testInitializedTableWithoutColumnsEmptySingleReference() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final VView view = VViewFactory.eINSTANCE.createView();
		domainElement = view;
		final TableControlHandle handle = createInitializedTableWithoutTableColumns();
		final VFeaturePathDomainModelReference domainModelReference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		domainModelReference.setDomainModelEFeature(VViewPackage.eINSTANCE.getView_RootEClass());
		handle.getTableControl().setDomainModelReference(domainModelReference);

		try {
			SWTViewTestHelper.render(handle.getTableControl(), domainElement, shell);
		} catch (final NullPointerException e) {
			fail("Fails with empty single reference in domain object");
		}

	}

	@Test
	public void testTableWithoutColumns() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		// setup model
		final TableControlHandle handle = createInitializedTableWithoutTableColumns();

		final Control render = SWTViewTestHelper.render(handle.getTableControl(), domainElement, shell);
		assertTrue(render instanceof Composite);

		assertEquals(domainElement.eClass().getEAttributes().size(),
			VTableDomainModelReference.class.cast(handle.getTableControl().getDomainModelReference())
				.getColumnDomainModelReferences().size());

		final Control control = getTable(render);
		assertTrue(control instanceof Table);
		final Table table = (Table) control;
		assertEquals(3, table.getColumnCount());
	}

	@Test
	public void testTableWithoutColumnsWithoutViewServices() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		final TableControlHandle handle = createInitializedTableWithoutTableColumns();
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(
			handle.getTableControl(),
			new ViewModelContextWithoutServices(handle.getTableControl()));

		final Control render = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (render == null) {
			fail();
		}
		assertTrue(render instanceof Composite);

		assertEquals(0, VTableDomainModelReference.class.cast(handle.getTableControl().getDomainModelReference())
			.getColumnDomainModelReferences().size());

		final Control control = getTable(render);
		assertTrue(control instanceof Table);
		final Table table = (Table) control;
		assertEquals(1, table.getColumnCount());
	}

	@Test
	public void testTableWithTwoColumns() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		// setup model
		final TableControlHandle handle = createTableWithTwoTableColumns();
		final Control render = SWTViewTestHelper.render(handle.getTableControl(), domainElement, shell);
		assertTrue(render instanceof Composite);

		final Control control = getTable(render);
		assertTrue(control instanceof Table);
		final Table table = (Table) control;
		assertEquals(2 + 1, table.getColumnCount());

	}

	@Test
	public void testTableWithTwoColumnsWithoutViewServices() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		// setup model
		final TableControlHandle handle = createTableWithTwoTableColumns();
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(
			handle.getTableControl(),
			new ViewModelContextWithoutServices(handle.getTableControl()));

		final Control render = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (render == null) {
			fail();
		}
		assertTrue(render instanceof Composite);

		final Control control = getTable(render);
		assertTrue(control instanceof Table);
		final Table table = (Table) control;
		assertEquals(2 + 1, table.getColumnCount());
	}

	@Test
	public void testTableWithTwoColumnsAdd() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		final TableControlHandle handle = createTableWithTwoTableColumns();
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(
			handle.getTableControl(),
			new ViewModelContextWithoutServices(handle.getTableControl()));

		final Control control = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (control == null) {
			fail("No control was rendered");
		}
		final Table table = (Table) getTable(control);
		assertEquals(1, table.getItemCount());
		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		((EClass) domainElement).getESuperTypes().add(eClass);
		assertEquals(2, table.getItemCount());
	}

	@Test
	public void testTableWithTwoColumnsRemove() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		final TableControlHandle handle = createTableWithTwoTableColumns();
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(
			handle.getTableControl(),
			new ViewModelContextWithoutServices(handle.getTableControl()));

		final Control control = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (control == null) {
			fail("No control was rendered");
		}
		final Table table = (Table) getTable(control);
		assertEquals(1, table.getItemCount());
		final EClass eClass = ((EClass) domainElement).getESuperTypes().get(0);
		((EClass) domainElement).getESuperTypes().remove(eClass);
		assertEquals(0, table.getItemCount());
	}

	@Test
	public void testTableWithTwoColumnsClear() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		((EClass) domainElement).getESuperTypes().add(eClass);
		final TableControlHandle handle = createTableWithTwoTableColumns();
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(
			handle.getTableControl(),
			new ViewModelContextWithoutServices(handle.getTableControl()));

		final Control control = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (control == null) {
			fail("No control was rendered");
		}
		final Table table = (Table) getTable(control);
		assertEquals(2, table.getItemCount());
		((EClass) domainElement).getESuperTypes().clear();
		assertEquals(0, table.getItemCount());
	}

	@Test
	public void testPanelTableWithTwoColumns() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		final EClass eClass = EcoreFactory.eINSTANCE.createEClass();
		((EClass) domainElement).getESuperTypes().add(eClass);
		final TableControlHandle handle = createTableWithTwoTableColumns();
		handle.getTableControl().setDetailEditing(DetailEditing.WITH_PANEL);
		handle.getTableControl().setDetailView(createDetailView());
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(
			handle.getTableControl(),
			new ViewModelContextWithoutServices(handle.getTableControl()));
		final Control render = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		final Control control = Composite.class.cast(render).getChildren()[0];
		if (control == null) {
			fail("No control was rendered");
		}
		final Composite controlComposite = (Composite) ((Composite) control).getChildren()[1];
		final Composite tableComposite = (Composite) controlComposite.getChildren()[0];
		final Table table = (Table) tableComposite.getChildren()[0];
		final ScrolledComposite scrolledComposite = (ScrolledComposite) controlComposite.getChildren()[1];
		final Composite parentForECPView = (Composite) scrolledComposite.getChildren()[0];
		assertEquals(2, table.getItemCount());
		final TableViewer tableViewer = getTableViewerFromRenderer(tableRenderer);

		// no initial selection
		assertEquals(0, parentForECPView.getChildren().length);

		// single selection
		tableViewer.setSelection(new StructuredSelection(table.getItem(0).getData()));
		assertEquals(1, parentForECPView.getChildren().length);
		final Composite viewComposite = (Composite) parentForECPView.getChildren()[0];
		final Composite detailComposite = (Composite) viewComposite.getChildren()[0];
		assertEquals(6, detailComposite.getChildren().length);

		// multi selection
		tableViewer.setSelection(new StructuredSelection(new Object[] { table.getItem(0).getData(),
			table.getItem(1).getData() }));
		assertEquals(0, parentForECPView.getChildren().length);

		// select again
		tableViewer.setSelection(new StructuredSelection(table.getItem(0).getData()));
		assertEquals(1, parentForECPView.getChildren().length);

		// no selection
		tableViewer.setSelection(new StructuredSelection());
		assertEquals(0, parentForECPView.getChildren().length);
	}

	@Test
	public void testTableSorting() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		// domain
		((EClass) domainElement).getESuperTypes().clear();
		final EClass class1 = createEClass("a", "b");
		final EClass class2 = createEClass("b", "c");
		final EClass class3 = createEClass("c", "a");
		((EClass) domainElement).getESuperTypes().add(class1);
		((EClass) domainElement).getESuperTypes().add(class2);
		((EClass) domainElement).getESuperTypes().add(class3);

		// table control
		final VTableControl tableControl = createTableControl();
		final VTableDomainModelReference tableDMR = (VTableDomainModelReference) tableControl.getDomainModelReference();
		tableDMR.setDomainModelEFeature(EcorePackage.eINSTANCE.getEClass_ESuperTypes());
		tableDMR.getColumnDomainModelReferences().add(createDMR(EcorePackage.eINSTANCE.getENamedElement_Name()));
		tableDMR.getColumnDomainModelReferences().add(
			createDMR(EcorePackage.eINSTANCE.getEClassifier_InstanceClassName()));

		// render
		final AbstractSWTRenderer<VElement> tableRenderer = rendererFactory.getRendererInstance(tableControl,
			new ViewModelContextWithoutServices(tableControl));
		final Control control = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (control == null) {
			fail("No control was rendered");
		}
		final Table table = SWTTestUtil.findControl(control, 0, Table.class);
		assertTableItemOrder(table, class1, class2, class3);

		// column 0 is validation column

		// select column 1
		// up
		SWTTestUtil.selectWidget(table.getColumns()[1]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class1, class2, class3);
		// down
		SWTTestUtil.selectWidget(table.getColumns()[1]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class3, class2, class1);
		// none
		SWTTestUtil.selectWidget(table.getColumns()[1]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class1, class2, class3);

		// select column 2
		// up
		SWTTestUtil.selectWidget(table.getColumns()[2]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class3, class1, class2);
		// down
		SWTTestUtil.selectWidget(table.getColumns()[2]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class2, class1, class3);
		// none
		SWTTestUtil.selectWidget(table.getColumns()[2]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class1, class2, class3);
	}

	@Test
	public void testTableSortingWithCellEditor() throws NoRendererFoundException, NoPropertyDescriptorFoundExeption,
		EMFFormsNoRendererException {
		// domain
		((EClass) domainElement).getESuperTypes().clear();
		final EClass class1 = createEClass("a", "b");
		final EClass class2 = createEClass("b", "c");
		final EClass class3 = createEClass("c", "a");
		((EClass) domainElement).getESuperTypes().add(class1);
		((EClass) domainElement).getESuperTypes().add(class2);
		((EClass) domainElement).getESuperTypes().add(class3);

		// table control
		final VTableControl tableControl = createTableControl();
		final VTableDomainModelReference tableDMR = (VTableDomainModelReference) tableControl.getDomainModelReference();
		tableDMR.setDomainModelEFeature(EcorePackage.eINSTANCE.getEClass_ESuperTypes());
		tableDMR.getColumnDomainModelReferences().add(createDMR(EcorePackage.eINSTANCE.getENamedElement_Name()));
		tableDMR.getColumnDomainModelReferences().add(
			createDMR(EcorePackage.eINSTANCE.getEClassifier_InstanceClassName()));

		// render
		final TableControlSWTRenderer tableRenderer = createRendererInstanceWithCustomCellEditor(tableControl);
		final Control control = tableRenderer.render(new SWTGridCell(0, 0, tableRenderer), shell);
		if (control == null) {
			fail("No control was rendered");
		}
		final Table table = SWTTestUtil.findControl(control, 0, Table.class);
		assertTableItemOrder(table, class1, class2, class3);

		// column 0 is validation column

		// select column 1
		// up
		SWTTestUtil.selectWidget(table.getColumns()[1]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class3, class2, class1);
		// down
		SWTTestUtil.selectWidget(table.getColumns()[1]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class1, class2, class3);
		// none
		SWTTestUtil.selectWidget(table.getColumns()[1]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class1, class2, class3);

		// select column 2
		// up
		SWTTestUtil.selectWidget(table.getColumns()[2]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class3, class1, class2);
		// down
		SWTTestUtil.selectWidget(table.getColumns()[2]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class2, class1, class3);
		// none
		SWTTestUtil.selectWidget(table.getColumns()[2]);
		SWTTestUtil.waitForUIThread();
		assertTableItemOrder(table, class1, class2, class3);
	}

	private TableControlSWTRenderer createRendererInstanceWithCustomCellEditor(final VTableControl tableControl)
		throws EMFFormsNoRendererException {
		final ViewModelContextWithoutServices viewModelContext = new ViewModelContextWithoutServices(tableControl);
		final EMFFormsContextProvider contextProvider = viewModelContext.getService(EMFFormsContextProvider.class);
		final IEclipseContext eclipseContext = contextProvider.getContext();
		final TableControlSWTRenderer tableControlSWTRenderer = new TableControlSWTRenderer(
			tableControl,
			viewModelContext,
			eclipseContext.get(ReportService.class),
			eclipseContext.get(EMFFormsDatabindingEMF.class),
			eclipseContext.get(EMFFormsLabelProvider.class),
			eclipseContext.get(VTViewTemplateProvider.class),
			eclipseContext.get(ImageRegistryService.class),
			eclipseContext.get(EMFFormsEditSupport.class)) {

			@Override
			protected CellEditor createCellEditor(EObject tempInstance, EStructuralFeature feature, Table table) {
				if (feature == EcorePackage.eINSTANCE.getENamedElement_Name()) {
					return new CompareCellEditor(table);
				}
				return super.createCellEditor(tempInstance, feature, table);
			}
		};
		tableControlSWTRenderer.init();
		return tableControlSWTRenderer;
	}

	private static void assertTableItemOrder(Table table, Object... objects) {
		assertEquals(objects.length, table.getItemCount());
		final TableItem[] items = table.getItems();
		for (int i = 0; i < items.length; i++) {
			assertSame(objects[i], items[i].getData());
		}
	}

	private static EClass createEClass(String name, String instanceClassName) {
		final EClass clazz = EcoreFactory.eINSTANCE.createEClass();
		clazz.setName(name);
		clazz.setInstanceClassName(instanceClassName);
		return clazz;
	}

	private static VFeaturePathDomainModelReference createDMR(EAttribute attribute, EReference... refs) {
		final VFeaturePathDomainModelReference dmr = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		dmr.setDomainModelEFeature(attribute);
		dmr.getDomainModelEReferencePath().addAll(Arrays.asList(refs));
		return dmr;
	}

	private VView createDetailView() {
		final VView detailView = VViewFactory.eINSTANCE.createView();
		final VControl name = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference nameRef = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		nameRef.setDomainModelEFeature(EcorePackage.eINSTANCE.getENamedElement_Name());
		name.setDomainModelReference(nameRef);
		detailView.getChildren().add(name);
		final VControl abstr = VViewFactory.eINSTANCE.createControl();
		final VFeaturePathDomainModelReference abstractRef = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		abstractRef.setDomainModelEFeature(EcorePackage.eINSTANCE.getEClass_Abstract());
		abstr.setDomainModelReference(abstractRef);
		detailView.getChildren().add(abstr);
		return detailView;

	}

	private Control getTable(Control render) {
		Composite composite = (Composite) render;
		composite = (Composite) composite.getChildren()[0];
		composite = (Composite) composite.getChildren()[1];
		// composite = (Composite) composite.getChildren()[0];
		// composite = (Composite) composite.getChildren()[0];
		// composite = (Composite) composite.getChildren()[0];
		return composite.getChildren()[0];
	}

	private static TableControlHandle createTableWithTwoTableColumns() {
		final TableControlHandle tableControlHandle = createInitializedTableWithoutTableColumns();
		final VDomainModelReference tableColumn1 = createTableColumn(EcorePackage.eINSTANCE.getEClass_Abstract());

		tableControlHandle.addFirstTableColumn(tableColumn1);
		final VDomainModelReference tableColumn2 = createTableColumn(EcorePackage.eINSTANCE.getEClass_Abstract());
		tableControlHandle.addSecondTableColumn(tableColumn2);
		return tableControlHandle;
	}

	public static VDomainModelReference createTableColumn(EStructuralFeature feature) {
		final VFeaturePathDomainModelReference reference = VViewFactory.eINSTANCE
			.createFeaturePathDomainModelReference();
		reference.setDomainModelEFeature(feature);
		return reference;
	}

	public static TableControlHandle createInitializedTableWithoutTableColumns() {
		final TableControlHandle tableControlHandle = createUninitializedTableWithoutColumns();
		final VFeaturePathDomainModelReference domainModelReference = VTableFactory.eINSTANCE
			.createTableDomainModelReference();
		domainModelReference.setDomainModelEFeature(EcorePackage.eINSTANCE.getEClass_ESuperTypes());
		tableControlHandle.getTableControl().setDomainModelReference(domainModelReference);

		return tableControlHandle;
	}

	public static TableControlHandle createUninitializedTableWithoutColumns() {
		final VTableControl tableControl = createTableControl();
		return new TableControlHandle(tableControl);
	}

	/**
	 * @return
	 */
	private static VTableControl createTableControl() {
		final VTableControl tc = VTableFactory.eINSTANCE.createTableControl();
		tc.setDomainModelReference(VTableFactory.eINSTANCE.createTableDomainModelReference());
		return tc;
	}

	private TableViewer getTableViewerFromRenderer(AbstractSWTRenderer<VElement> renderer) {
		try {
			final Method method = TableControlSWTRenderer.class.getDeclaredMethod("getTableViewer");
			method.setAccessible(true);
			return (TableViewer) method.invoke(renderer);
		} catch (final NoSuchMethodException ex) {
			fail(ex.getMessage());
		} catch (final SecurityException ex) {
			fail(ex.getMessage());
		} catch (final IllegalAccessException ex) {
			fail(ex.getMessage());
		} catch (final IllegalArgumentException ex) {
			fail(ex.getMessage());
		} catch (final InvocationTargetException ex) {
			fail(ex.getMessage());
		}
		return null;
	}

	/**
	 * Stub implementation without getting services from ex. point.
	 *
	 * @author jfaltermeier
	 *
	 */
	private class ViewModelContextWithoutServices implements ViewModelContext {

		private final VElement view;
		private final EMFFormsContextProvider contextProvider;

		@SuppressWarnings("restriction")
		ViewModelContextWithoutServices(VElement view) {
			this.view = view;
			contextProvider = new org.eclipse.emfforms.internal.swt.core.di.EMFFormsContextProviderImpl();
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getViewModel()
		 */
		@Override
		public VElement getViewModel() {
			return view;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getDomainModel()
		 */
		@Override
		public EObject getDomainModel() {
			return domainElement;
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerViewChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void registerViewChangeListener(ModelChangeListener modelChangeListener) {
			// not needed
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#unregisterViewChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void unregisterViewChangeListener(ModelChangeListener modelChangeListener) {
			// not needed
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerDomainChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void registerDomainChangeListener(ModelChangeListener modelChangeListener) {
			// not needed
		}

		/**
		 *
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#unregisterDomainChangeListener(org.eclipse.emf.ecp.view.spi.model.ModelChangeListener)
		 */
		@Override
		public void unregisterDomainChangeListener(ModelChangeListener modelChangeListener) {
			// not needed
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#dispose()
		 */
		@Override
		public void dispose() {
			// not needed
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#hasService(java.lang.Class)
		 */
		@Override
		public <T> boolean hasService(Class<T> serviceType) {
			if (EMFFormsContextProvider.class.equals(serviceType)) {
				return true;
			}
			return false;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getService(java.lang.Class)
		 */
		@SuppressWarnings("unchecked")
		@Override
		public <T> T getService(Class<T> serviceType) {
			if (EMFFormsContextProvider.class.equals(serviceType)) {
				return (T) contextProvider;
			}
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(org.eclipse.emf.ecore.EStructuralFeature.Setting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VControl> getControlsFor(Setting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getControlsFor(org.eclipse.emf.ecp.common.spi.UniqueSetting)
		 * @deprecated
		 */
		@Deprecated
		@Override
		public Set<VElement> getControlsFor(UniqueSetting setting) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getContextValue(java.lang.String)
		 */
		@Override
		public Object getContextValue(String key) {
			return null;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#putContextValue(java.lang.String,
		 *      java.lang.Object)
		 */
		@Override
		public void putContextValue(String key, Object value) {

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#registerDisposeListener(org.eclipse.emf.ecp.view.spi.context.ViewModelContextDisposeListener)
		 */
		@Override
		public void registerDisposeListener(ViewModelContextDisposeListener listener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#addContextUser(java.lang.Object)
		 */
		@Override
		public void addContextUser(Object user) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#removeContextUser(java.lang.Object)
		 */
		@Override
		public void removeContextUser(Object user) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getChildContext(org.eclipse.emf.ecore.EObject,
		 *      org.eclipse.emf.ecp.view.spi.model.VElement, org.eclipse.emf.ecp.view.spi.model.VView,
		 *      org.eclipse.emf.ecp.view.spi.context.ViewModelService[])
		 */
		@Override
		public ViewModelContext getChildContext(EObject eObject, VElement parent, VView vView,
			ViewModelService... viewModelServices) {
			// TODO Auto-generated method stub
			final ViewModelContextWithoutServices vmcws = new ViewModelContextWithoutServices(vView);

			return vmcws;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void registerEMFFormsContextListener(EMFFormsContextListener contextListener) {
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterEMFFormsContextListener(org.eclipse.emfforms.spi.core.services.view.EMFFormsContextListener)
		 */
		@Override
		public void unregisterEMFFormsContextListener(EMFFormsContextListener contextListener) {
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getParentContext()
		 */
		@Override
		public ViewModelContext getParentContext() {
			// TODO Auto-generated method stub
			return null;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#changeDomainModel(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public void changeDomainModel(EObject newDomainModel) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#registerRootDomainModelChangeListener(org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener)
		 */
		@Override
		public void registerRootDomainModelChangeListener(RootDomainModelChangeListener rootDomainModelChangeListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emfforms.spi.core.services.view.EMFFormsViewContext#unregisterRootDomainModelChangeListener(org.eclipse.emfforms.spi.core.services.view.RootDomainModelChangeListener)
		 */
		@Override
		public void unregisterRootDomainModelChangeListener(
			RootDomainModelChangeListener rootDomainModelChangeListener) {
			// TODO Auto-generated method stub

		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.emf.ecp.view.spi.context.ViewModelContext#getParentVElement()
		 */
		@Override
		public VElement getParentVElement() {
			// TODO Auto-generated method stub
			return null;
		}

	}

	private static class PrintStreamWrapper extends PrintStream {

		private final PrintStream printStream;

		PrintStreamWrapper(PrintStream printStream) {
			super(new ByteArrayOutputStream());
			this.printStream = printStream;
		}

		/**
		 * {@inheritDoc}
		 *
		 * @see java.io.PrintStream#print(java.lang.String)
		 */
		@Override
		public void print(String s) {
			log = log.concat("\n" + s);
			printStream.print(s + "\n");
		}
	}

	private static class CompareCellEditor extends StringCellEditor implements ECPCellEditorComparator {

		CompareCellEditor(Table table) {
			super(table);
		}

		@Override
		public int compare(Object e1, Object e2, int direction) {
			final String value1 = String.class.cast(e1);
			final String value2 = String.class.cast(e2);
			int result;
			if (value1 == null) {
				result = 1;
			} else if (value2 == null) {
				result = -1;
			} else {
				result = value1.toString().compareTo(value2.toString()) * -1;// we flip the oder in our custom
																				// comparator
			}
			// If descending order, flip the direction
			if (direction == 2) {
				result = -result;
			}
			return result;
		}

	}

}
