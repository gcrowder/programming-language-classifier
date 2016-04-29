/**
 *
 */
package org.eclipse.emf.ecp.view.treemasterdetail.ui.swt.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.eclipse.emf.ecp.ui.view.ECPRendererException;
import org.eclipse.emf.ecp.ui.view.swt.ECPSWTViewRenderer;
import org.eclipse.emf.ecp.view.internal.context.ViewModelContextImpl;
import org.eclipse.emf.ecp.view.internal.swt.ContextMenuViewModelService;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.test.common.swt.spi.SWTViewTestHelper;
import org.eclipse.emf.ecp.view.treemasterdetail.model.VTreeMasterDetail;
import org.eclipse.emf.ecp.view.treemasterdetail.model.VTreeMasterDetailFactory;
import org.eclipse.emf.emfstore.bowling.BowlingFactory;
import org.eclipse.emf.emfstore.bowling.Player;
import org.eclipse.swt.widgets.Shell;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Alexandra Buzila
 *
 */
@SuppressWarnings({ "restriction", "deprecation" })
public class TreeMasterDetailContextMenuViewModelService_PTest {

	private VView view;
	private Shell shell;
	private VView detailView;
	private Player player;
	private CountDownLatch latch;

	@Before
	public void createDomainObject() {
		player = BowlingFactory.eINSTANCE.createPlayer();
		shell = SWTViewTestHelper.createShell();
		view = VViewFactory.eINSTANCE.createView();
		detailView = VViewFactory.eINSTANCE.createView();
		final VTreeMasterDetail treeMasterDetail = VTreeMasterDetailFactory.eINSTANCE.createTreeMasterDetail();
		treeMasterDetail.setDetailView(detailView);
		view.getChildren().add(treeMasterDetail);
	}

	@Test
	public void testTreeMasterDetailNoContextMenuService() {
		try {
			latch = new CountDownLatch(1);
			final ViewModelContextImpl viewContext = new ViewModelContextImpl(view, player);
			ECPSWTViewRenderer.INSTANCE.render(shell, viewContext);
			assertFalse("registerContextMenu was called", latch.await(1, TimeUnit.SECONDS)); //$NON-NLS-1$
		} catch (final ECPRendererException e) {
			e.printStackTrace();
		} catch (final InterruptedException e) {
			e.printStackTrace();
		}
	}

	@Test
	public void testTreeMasterDetailWithContextMenuService() {
		try {
			latch = new CountDownLatch(1);
			final TestContextMenuViewModelService service = new TestContextMenuViewModelService();
			final ViewModelContextImpl viewContext = new ViewModelContextImpl(view, player, service);
			ECPSWTViewRenderer.INSTANCE.render(shell, viewContext);
			assertTrue("Context menu was not registered", latch.await(5, TimeUnit.SECONDS)); //$NON-NLS-1$
		} catch (final ECPRendererException e) {
			e.printStackTrace();
		} catch (final InterruptedException e) {
			e.printStackTrace();
		}
	}

	private class TestContextMenuViewModelService extends ContextMenuViewModelService {

		@Override
		public void instantiate(ViewModelContext context) {
			// TODO Auto-generated method stub

		}

		@Override
		public void dispose() {
			// TODO Auto-generated method stub

		}

		@Override
		public int getPriority() {
			// TODO Auto-generated method stub
			return 0;
		}

		@Override
		public boolean registerContextMenu() {
			latch.countDown();
			return true;
		}

	}
}
