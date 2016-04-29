/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Johannes Faltermeier - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.validation.test;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecp.test.common.DefaultRealm;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContext;
import org.eclipse.emf.ecp.view.spi.context.ViewModelContextFactory;
import org.eclipse.emf.ecp.view.spi.model.VControl;
import org.eclipse.emf.ecp.view.spi.model.VDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VFeaturePathDomainModelReference;
import org.eclipse.emf.ecp.view.spi.model.VView;
import org.eclipse.emf.ecp.view.spi.model.VViewFactory;
import org.eclipse.emf.ecp.view.spi.table.model.VTableControl;
import org.eclipse.emf.ecp.view.spi.table.model.VTableDomainModelReference;
import org.eclipse.emf.ecp.view.spi.table.model.VTableFactory;
import org.eclipse.emf.ecp.view.validation.test.model.Book;
import org.eclipse.emf.ecp.view.validation.test.model.Librarian;
import org.eclipse.emf.ecp.view.validation.test.model.Library;
import org.eclipse.emf.ecp.view.validation.test.model.TestFactory;
import org.eclipse.emf.ecp.view.validation.test.model.TestPackage;
import org.eclipse.emf.ecp.view.validation.test.model.Writer;
import org.junit.After;
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
public class ValidationPerformance_ITest {
	private DefaultRealm defaultRealm;

	private Library domain;
	private VView view;
	private long testBegin;
	private long testEnd;

	private final int domainObjectCountFactor;
	private final int viewModelCountFactor;
	private final long timeGateStartup;
	private final long timeGateAdd;
	private int loops = 100;

	public ValidationPerformance_ITest(int domainObjectCountFactor, int viewModelCountFactor, long timeGateStartup,
		long timeGateAdd) {
		this.domainObjectCountFactor = domainObjectCountFactor;
		this.viewModelCountFactor = viewModelCountFactor;
		this.timeGateStartup = timeGateStartup;
		this.timeGateAdd = timeGateAdd;
	}

	@Parameters
	public static Collection<Object[]> data() {
		final List<Object[]> data = new ArrayList<Object[]>();
		// #domainObjects, #viewmodels, init, add
		data.add(createParameters(4, 1, 100, 100)); // 0
		data.add(createParameters(8, 1, 100, 100));// 1
		data.add(createParameters(11, 1, 100, 200));// 2

		data.add(createParameters(4, 5, 100, 200));// 3
		data.add(createParameters(8, 5, 150, 400));// 4
		data.add(createParameters(11, 5, 300, 700));// 5

		data.add(createParameters(4, 10, 100, 400));// 6
		data.add(createParameters(8, 10, 500, 800));// 7
		data.add(createParameters(11, 10, 1000, 1500));// 8

		return data;
	}

	/**
	 * Creates the parameters that will be passed to the constructor before every execution.
	 *
	 * @param domainObjectCountFactor The factor used for scaling up the amount of domains objects.
	 * @param viewModelCountFactor The factors used for scaling up the amount of controls in the view model.
	 * @param timeGateStartup the timegate used for the startup of the context with the validation service
	 * @param timeGateAdd the timegate used for assertions when an element is added to a multireference
	 * @return the parameters
	 */
	private static Object[] createParameters(int domainObjectCountFactor, int viewModelCountFactor,
		long timeGateStartup, long timeGateAdd) {
		return new Object[] { domainObjectCountFactor, viewModelCountFactor, timeGateStartup, timeGateAdd };
	}

	@Before
	public void before() {
		defaultRealm = new DefaultRealm();
		System.out.println();
		domain = createLibrary();
		view = createView();
	}

	@After
	public void tearDown() {
		defaultRealm.dispose();
	}

	private long now() {
		return System.currentTimeMillis();
	}

	private ViewModelContext createContext() {
		return ViewModelContextFactory.INSTANCE.createViewModelContext(view, domain);
	}

	private long diff() {
		return testEnd - testBegin;
	}

	private Library createLibrary() {
		final Library library = createLibraryWithLibrarian();
		for (int i = 0; i < domainObjectCountFactor; i++) {
			addWriterWithBooks(library, true, domainObjectCountFactor, domainObjectCountFactor);
			addWriterWithBooks(library, false, domainObjectCountFactor, domainObjectCountFactor);
			for (int j = 0; j < domainObjectCountFactor; j++) {
				library.getWriters().add(createWriter(true));
				library.getWriters().add(createWriter(false));
				library.getBooks().add(createBook(true));
				library.getBooks().add(createBook(false));
			}
		}
		return library;
	}

	private Library createLibraryWithLibrarian() {
		final Library library = TestFactory.eINSTANCE.createLibrary();
		final Librarian librarian = TestFactory.eINSTANCE.createLibrarian();
		library.setLibrarian(librarian);
		return library;
	}

	private Book createBook(boolean withTitle) {
		final Book book = TestFactory.eINSTANCE.createBook();
		if (withTitle) {
			book.setTitle("Title");
		}
		return book;
	}

	private Writer createWriter(boolean withFirstName) {
		final Writer writer = TestFactory.eINSTANCE.createWriter();
		if (withFirstName) {
			writer.setFirstName("Name");
		}
		return writer;
	}

	private void addWriterWithBooks(Library library, boolean hasFirstName, int bookWithTitleCount,
		int bookWithoutTitleCount) {
		final Writer writer = createWriter(hasFirstName);
		library.getWriters().add(writer);
		List<Book> books = new ArrayList<Book>();
		for (int i = 0; i < bookWithTitleCount; i++) {
			books.add(createBook(true));
		}
		library.getBooks().addAll(books);
		writer.getBooks().addAll(books);
		books = new ArrayList<Book>();
		for (int i = 0; i < bookWithoutTitleCount; i++) {
			books.add(createBook(false));
		}
		library.getBooks().addAll(books);
		writer.getBooks().addAll(books);
	}

	private VView createView() {
		final VView view = VViewFactory.eINSTANCE.createView();
		view.setRootEClass(TestPackage.eINSTANCE.getLibrary());

		for (int i = 0; i < viewModelCountFactor; i++) {
			final VControl nameControl = VViewFactory.eINSTANCE.createControl();
			final VFeaturePathDomainModelReference nameReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			nameReference.setDomainModelEFeature(TestPackage.eINSTANCE.getLibrary_Name());
			nameControl.setDomainModelReference(nameReference);
			view.getChildren().add(nameControl);

			final VTableControl writersTable = VTableFactory.eINSTANCE.createTableControl();
			final VTableDomainModelReference writerReference = VTableFactory.eINSTANCE
				.createTableDomainModelReference();
			writerReference.setDomainModelEFeature(TestPackage.eINSTANCE.getLibrary_Writers());
			writersTable.setDomainModelReference(writerReference);
			VTableDomainModelReference.class.cast(writersTable.getDomainModelReference())
				.getColumnDomainModelReferences().add(createTableColumn(TestPackage.eINSTANCE.getWriter_BirthDate()));
			VTableDomainModelReference.class.cast(writersTable.getDomainModelReference())
				.getColumnDomainModelReferences().add(createTableColumn(TestPackage.eINSTANCE.getWriter_EMail()));
			VTableDomainModelReference.class.cast(writersTable.getDomainModelReference())
				.getColumnDomainModelReferences().add(createTableColumn(TestPackage.eINSTANCE.getWriter_FirstName()));
			VTableDomainModelReference.class.cast(writersTable.getDomainModelReference())
				.getColumnDomainModelReferences().add(createTableColumn(TestPackage.eINSTANCE.getWriter_LastName()));
			VTableDomainModelReference.class.cast(writersTable.getDomainModelReference())
				.getColumnDomainModelReferences().add(createTableColumn(TestPackage.eINSTANCE.getWriter_Pseudonym()));
			view.getChildren().add(writersTable);

			// final VControl writersControl = VViewFactory.eINSTANCE.createControl();
			// final VFeaturePathDomainModelReference writersReference = VViewFactory.eINSTANCE
			// .createFeaturePathDomainModelReference();
			// writersReference.setDomainModelEFeature(TestPackage.eINSTANCE.getLibrary_Writers());
			// writersControl.setDomainModelReference(writersReference);
			// view.getChildren().add(writersControl);

			final VControl booksControl = VViewFactory.eINSTANCE.createControl();
			final VFeaturePathDomainModelReference booksReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			booksReference.setDomainModelEFeature(TestPackage.eINSTANCE.getLibrary_Books());
			booksControl.setDomainModelReference(booksReference);
			view.getChildren().add(booksControl);

			final VControl librarianControl = VViewFactory.eINSTANCE.createControl();
			final VFeaturePathDomainModelReference librarianReference = VViewFactory.eINSTANCE
				.createFeaturePathDomainModelReference();
			librarianReference.setDomainModelEFeature(TestPackage.eINSTANCE.getLibrary_Librarian());
			librarianControl.setDomainModelReference(librarianReference);
			view.getChildren().add(librarianControl);
		}

		return view;
	}

	private VDomainModelReference createTableColumn(EAttribute attribute) {
		final VFeaturePathDomainModelReference column = VViewFactory.eINSTANCE.createFeaturePathDomainModelReference();
		column.setDomainModelEFeature(attribute);
		return column;
	}

	@Test
	public void testViewModelContextStartup() {
		loops = 50;
		final List<Long> diffs = new ArrayList<Long>();
		countDomainObjects();
		for (int i = 0; i < loops; i++) {
			testBegin = now();
			final ViewModelContext context = createContext();
			testEnd = now();
			diffs.add(diff());
			context.dispose();
		}
		long allDiffs = 0;
		for (final Long diff : diffs) {
			allDiffs = allDiffs + diff;
		}
		final long avgDiff = allDiffs / loops;
		System.out.print("Init:" + avgDiff);
		assertTrue(avgDiff < timeGateStartup);
	}

	@Test
	public void testAddWriterWithError() {
		countDomainObjects();
		final List<Long> diffs = new ArrayList<Long>();
		for (int i = 0; i < loops; i++) {
			final ViewModelContext context = createContext();
			testBegin = now();
			domain.getWriters().add(createWriter(false));
			testEnd = now();
			diffs.add(diff());
			context.dispose();
		}
		long allDiffs = 0;
		for (final Long diff : diffs) {
			allDiffs = allDiffs + diff;
		}
		final long avgDiff = allDiffs / loops;
		System.out.print("Add:" + avgDiff);
		assertTrue(avgDiff < timeGateAdd);
	}

	/**
	 *
	 */
	private void countDomainObjects() {
		final TreeIterator<EObject> allContents = domain.eAllContents();
		int i = 0;
		while (allContents.hasNext()) {
			i++;
			allContents.next();
		}
		System.out.print(i + ",");
	}
}
