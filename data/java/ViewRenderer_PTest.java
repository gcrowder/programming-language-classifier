package org.eclipse.emf.ecp.view.internal.core.swt.renderer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VContainedElement;
import org.eclipse.emf.ecp.view.spi.model.VElement;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.renderer.NoPropertyDescriptorFoundExeption;
import org.eclipse.emf.ecp.view.spi.renderer.NoRendererFoundException;
import org.eclipse.emfforms.spi.common.locale.EMFFormsLocaleProvider;
import org.eclipse.emfforms.spi.common.report.ReportService;
import org.eclipse.emfforms.spi.core.services.databinding.EMFFormsDatabinding;
import org.eclipse.emfforms.spi.swt.core.AbstractSWTRenderer;
import org.eclipse.emfforms.spi.swt.core.EMFFormsNoRendererException;
import org.eclipse.emfforms.spi.swt.core.EMFFormsRendererFactory;
import org.eclipse.emfforms.spi.swt.core.layout.GridDescriptionFactory;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridCell;
import org.eclipse.emfforms.spi.swt.core.layout.SWTGridDescription;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

public class ViewRenderer_PTest {

	private ViewSWTRenderer viewRenderer;
	private VView view;
	private ViewModelContext context;
	private Shell shell;
	private EMFFormsRendererFactory rendererFactory;
	private DefaultRealm realm;

	@BeforeClass
	public static void setupClass() {
	}

	@Before
	public void setUp() {
		realm = new DefaultRealm();
		final ReportService reportService = mock(ReportService.class);
		final EMFFormsDatabinding databindingService = mock(EMFFormsDatabinding.class);
		rendererFactory = mock(EMFFormsRendererFactory.class);
		final EMFFormsLocaleProvider localeProvider = mock(EMFFormsLocaleProvider.class);
		view = Mockito.mock(VView.class);

		context = Mockito.mock(ViewModelContext.class);
		shell = new Shell();
		viewRenderer = new ViewSWTRenderer(view, context, reportService, rendererFactory, databindingService,
			localeProvider);
		viewRenderer.init();
	}

	@After
	public void tearDown() {
		realm.dispose();
		shell.dispose();
	}

	@Test
	public void testGridDescription() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		final SWTGridDescription gridDescription = viewRenderer.getGridDescription(GridDescriptionFactory.INSTANCE
			.createEmptyGridDescription());
		assertEquals(1, gridDescription.getColumns());
		assertEquals(1, gridDescription.getRows());
	}

	@Test
	public void testEmptyView() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption {
		when(view.getChildren())
			.thenReturn(new BasicEList<VContainedElement>());
		final Control render = viewRenderer.render(new SWTGridCell(0, 0, viewRenderer), shell);
		assertTrue(Composite.class.isInstance(render));
		assertEquals(0, Composite.class.cast(render).getChildren().length);
	}

	@Test
	public void testMultipleSimpleCompositeView() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		final BasicEList<VContainedElement> basicEList = new BasicEList<VContainedElement>();
		final VContainedElement control1 = mock(VContainedElement.class);
		final VContainedElement control2 = mock(VContainedElement.class);
		basicEList.add(control1);
		basicEList.add(control2);
		when(view.getChildren()).thenReturn(basicEList);

		final AbstractSWTRenderer<VElement> mockRenderer1 = createCompositeMockRenderer(control1, 1);
		final AbstractSWTRenderer<VElement> mockRenderer2 = createCompositeMockRenderer(control2, 1);

		when(rendererFactory.getRendererInstance(control1, context)).thenReturn(mockRenderer1);
		when(rendererFactory.getRendererInstance(control2, context)).thenReturn(mockRenderer2);

		final Control render = viewRenderer.render(new SWTGridCell(0, 0, viewRenderer), shell);
		assertTrue(Composite.class.isInstance(render));
		assertEquals(2, Composite.class.cast(render).getChildren().length);
		assertTrue(GridData.class.isInstance(Composite.class.cast(render).getChildren()[0].getLayoutData()));
		assertTrue(GridData.class.isInstance(Composite.class.cast(render).getChildren()[1].getLayoutData()));

		assertEquals(1,
			GridData.class.cast(Composite.class.cast(render).getChildren()[0].getLayoutData()).horizontalSpan);
		assertEquals(1,
			GridData.class.cast(Composite.class.cast(render).getChildren()[1].getLayoutData()).horizontalSpan);
	}

	@Test
	public void testMultipleComplexGridDescriptionView() throws NoRendererFoundException,
		NoPropertyDescriptorFoundExeption, EMFFormsNoRendererException {
		final BasicEList<VContainedElement> basicEList = new BasicEList<VContainedElement>();
		final VContainedElement control1 = mock(VContainedElement.class);
		final VContainedElement control2 = mock(VContainedElement.class);
		final VContainedElement control3 = mock(VContainedElement.class);
		basicEList.add(control1);
		basicEList.add(control2);
		basicEList.add(control3);
		when(view.getChildren()).thenReturn(basicEList);

		final AbstractSWTRenderer<VElement> mockRenderer1 = createCompositeMockRenderer(control1, 1);
		final AbstractSWTRenderer<VElement> mockRenderer2 = createCompositeMockRenderer(control2, 3);
		final AbstractSWTRenderer<VElement> mockRenderer3 = createCompositeMockRenderer(control3, 2);

		when(rendererFactory.getRendererInstance(control1, context)).thenReturn(mockRenderer1);
		when(rendererFactory.getRendererInstance(control2, context)).thenReturn(mockRenderer2);
		when(rendererFactory.getRendererInstance(control3, context)).thenReturn(mockRenderer3);

		final Control render = viewRenderer.render(new SWTGridCell(0, 0, viewRenderer), shell);
		assertTrue(Composite.class.isInstance(render));
		assertEquals(6, Composite.class.cast(render).getChildren().length);
		for (int i = 0; i < 6; i++) {
			assertTrue(GridData.class.isInstance(Composite.class.cast(render).getChildren()[i].getLayoutData()));
		}

		assertEquals(3,
			GridData.class.cast(Composite.class.cast(render).getChildren()[0].getLayoutData()).horizontalSpan);
		assertEquals(2,
			GridData.class.cast(Composite.class.cast(render).getChildren()[5].getLayoutData()).horizontalSpan);
	}

	private AbstractSWTRenderer<VElement> createCompositeMockRenderer(
		VContainedElement control1, int numColumns) throws NoRendererFoundException, NoPropertyDescriptorFoundExeption {
		@SuppressWarnings("unchecked")
		final AbstractSWTRenderer<VElement> mockRenderer = mock(AbstractSWTRenderer.class);

		final SWTGridDescription gd = GridDescriptionFactory.INSTANCE.createSimpleGrid(1, numColumns, mockRenderer);
		when(mockRenderer.getGridDescription(any(SWTGridDescription.class))).thenAnswer(
			new Answer<SWTGridDescription>() {
				@Override
				public SWTGridDescription answer(InvocationOnMock invocation)
					throws Throwable {
					final Object[] args = invocation.getArguments();
					final SWTGridDescription desc = (SWTGridDescription) args[0];
					if (desc.getColumns() == 0 && desc.getRows() == 0 && desc.getGrid() == null) {
						return gd;
					}
					return null;
				}
			});
		when(mockRenderer.render(any(SWTGridCell.class), any(Composite.class))).thenAnswer(new Answer<Control>() {

			@Override
			public Control answer(InvocationOnMock invocation) throws Throwable {
				final Object[] args = invocation.getArguments();
				return new Composite((Composite) args[1], SWT.NONE);
			}

		});
		return mockRenderer;
	}

}
