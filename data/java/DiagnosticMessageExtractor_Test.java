/*******************************************************************************
 * Copyright (c) 2011-2015 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Eugen Neufeld - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.view.spi.model;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.eclipse.emf.common.util.Diagnostic;
import org.junit.Test;

/**
 * Test cases for DiagnosticMessageExtractor.
 *
 * @author Eugen Neufeld
 *
 */
public class DiagnosticMessageExtractor_Test {

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Diagnostic)}.
	 */
	@Test
	public void testGetMessageSingleDiagnosticNoChildren() {
		final Diagnostic diagnostic = mock(Diagnostic.class);
		when(diagnostic.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticMessage = "Dummy Message"; //$NON-NLS-1$
		when(diagnostic.getMessage()).thenReturn(diagnosticMessage);
		final String message = DiagnosticMessageExtractor.getMessage(diagnostic);
		assertEquals(diagnosticMessage, message);
	}

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Diagnostic)}.
	 */
	@Test
	public void testGetMessageSingleDiagnosticMultipleChildren() {
		final Diagnostic diagnostic = mock(Diagnostic.class);
		when(diagnostic.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticTopMessage = "Top Message"; //$NON-NLS-1$
		when(diagnostic.getMessage()).thenReturn(diagnosticTopMessage);
		final Diagnostic diagnosticChild1 = mock(Diagnostic.class);
		final String diagnosticChildMessage1 = "Dummy Message 1"; //$NON-NLS-1$
		when(diagnosticChild1.getMessage()).thenReturn(diagnosticChildMessage1);
		final Diagnostic diagnosticChild2 = mock(Diagnostic.class);
		final String diagnosticChildMessage2 = "Dummy Message 2"; //$NON-NLS-1$
		when(diagnosticChild2.getMessage()).thenReturn(diagnosticChildMessage2);
		when(diagnostic.getChildren()).thenReturn(Arrays.asList(diagnosticChild1, diagnosticChild2));

		final String message = DiagnosticMessageExtractor.getMessage(diagnostic);
		final String expectedMessage = diagnosticChildMessage1 + "\n" + diagnosticChildMessage2; //$NON-NLS-1$
		assertEquals(expectedMessage, message);
	}

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Collection)}.
	 */
	@Test
	public void testGetMessageDiagnosticSameSeverityCollection() {
		final Diagnostic diagnostic1 = mock(Diagnostic.class);
		when(diagnostic1.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticChildMessage1 = "Dummy Message 1"; //$NON-NLS-1$
		when(diagnostic1.getMessage()).thenReturn(diagnosticChildMessage1);
		final Diagnostic diagnostic2 = mock(Diagnostic.class);
		when(diagnostic2.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticChildMessage2 = "Dummy Message 2"; //$NON-NLS-1$
		when(diagnostic2.getMessage()).thenReturn(diagnosticChildMessage2);
		final Diagnostic diagnostic3 = mock(Diagnostic.class);
		when(diagnostic3.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticChildMessage3 = "Dummy Message 3"; //$NON-NLS-1$
		when(diagnostic3.getMessage()).thenReturn(diagnosticChildMessage3);
		final Set<Diagnostic> diagnostics = new LinkedHashSet<Diagnostic>();
		diagnostics.add(diagnostic1);
		diagnostics.add(diagnostic2);
		diagnostics.add(diagnostic3);
		final String message = DiagnosticMessageExtractor.getMessage(diagnostics);
		final String expectedMessage = diagnosticChildMessage1
			+ "\n" + diagnosticChildMessage2 + "\n" + diagnosticChildMessage3; //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(expectedMessage, message);
	}

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Collection)}.
	 */
	@Test
	public void testGetMessageDiagnosticDifferentSeverityCollection() {
		final Diagnostic diagnostic1 = mock(Diagnostic.class);
		when(diagnostic1.getSeverity()).thenReturn(Diagnostic.WARNING);
		final String diagnosticChildMessage1 = "Dummy Message 1"; //$NON-NLS-1$
		when(diagnostic1.getMessage()).thenReturn(diagnosticChildMessage1);
		final Diagnostic diagnostic2 = mock(Diagnostic.class);
		when(diagnostic2.getSeverity()).thenReturn(Diagnostic.INFO);
		final String diagnosticChildMessage2 = "Dummy Message 2"; //$NON-NLS-1$
		when(diagnostic2.getMessage()).thenReturn(diagnosticChildMessage2);
		final Diagnostic diagnostic3 = mock(Diagnostic.class);
		when(diagnostic3.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticChildMessage3 = "Dummy Message 3"; //$NON-NLS-1$
		when(diagnostic3.getMessage()).thenReturn(diagnosticChildMessage3);

		final Diagnostic diagnostic4 = mock(Diagnostic.class);
		when(diagnostic4.getSeverity()).thenReturn(Diagnostic.OK);
		final String diagnosticChildMessage4 = "Dummy Message 4"; //$NON-NLS-1$
		when(diagnostic4.getMessage()).thenReturn(diagnosticChildMessage4);

		final Set<Diagnostic> diagnostics = new LinkedHashSet<Diagnostic>();
		diagnostics.add(diagnostic1);
		diagnostics.add(diagnostic2);
		diagnostics.add(diagnostic3);
		diagnostics.add(diagnostic4);
		final String message = DiagnosticMessageExtractor.getMessage(diagnostics);
		final String expectedMessage = diagnosticChildMessage3
			+ "\n" + diagnosticChildMessage1 + "\n" + diagnosticChildMessage2; //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(expectedMessage, message);
	}

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Collection)}.
	 */
	@Test
	public void testGetMessageDiagnosticSameSeverityWithChildrenCollection() {
		final Diagnostic diagnostic1 = mock(Diagnostic.class);
		when(diagnostic1.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticMessage1 = "Dummy Message 1"; //$NON-NLS-1$
		when(diagnostic1.getMessage()).thenReturn(diagnosticMessage1);
		final Diagnostic diagnostic2 = mock(Diagnostic.class);
		when(diagnostic2.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticMessage2 = "Dummy Message 2"; //$NON-NLS-1$
		when(diagnostic2.getMessage()).thenReturn(diagnosticMessage2);

		final Diagnostic diagnosticChild1 = mock(Diagnostic.class);
		final String diagnosticChildMessage1 = "Dummy Child Message 1"; //$NON-NLS-1$
		when(diagnosticChild1.getMessage()).thenReturn(diagnosticChildMessage1);
		final Diagnostic diagnosticChild2 = mock(Diagnostic.class);
		final String diagnosticChildMessage2 = "Dummy Child Message 2"; //$NON-NLS-1$
		when(diagnosticChild2.getMessage()).thenReturn(diagnosticChildMessage2);
		when(diagnostic2.getChildren()).thenReturn(Arrays.asList(diagnosticChild1, diagnosticChild2));

		final Set<Diagnostic> diagnostics = new LinkedHashSet<Diagnostic>();
		diagnostics.add(diagnostic1);
		diagnostics.add(diagnostic2);
		final String message = DiagnosticMessageExtractor.getMessage(diagnostics);
		final String expectedMessage = diagnosticMessage1
			+ "\n" + diagnosticChildMessage1 + "\n" + diagnosticChildMessage2; //$NON-NLS-1$ //$NON-NLS-2$
		assertEquals(expectedMessage, message);
	}

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Diagnostic)}.
	 */
	@Test
	public void testGetMessageDiagnosticSingleDiagnosticCollection() {
		final Diagnostic diagnostic = mock(Diagnostic.class);
		when(diagnostic.getSeverity()).thenReturn(Diagnostic.ERROR);
		final String diagnosticMessage = "Dummy Message"; //$NON-NLS-1$
		when(diagnostic.getMessage()).thenReturn(diagnosticMessage);
		final String message = DiagnosticMessageExtractor.getMessage(Arrays.asList(diagnostic));
		assertEquals(diagnosticMessage, message);
	}

	/**
	 * Test method for {@link DiagnosticMessageExtractor#getMessage(Diagnostic)}.
	 */
	@Test
	public void testGetMessageSingleDiagnosticOK() {
		final Diagnostic diagnostic = mock(Diagnostic.class);
		when(diagnostic.getSeverity()).thenReturn(Diagnostic.OK);
		final String diagnosticMessage = "Dummy Message"; //$NON-NLS-1$
		when(diagnostic.getMessage()).thenReturn(diagnosticMessage);
		final String message = DiagnosticMessageExtractor.getMessage(Arrays.asList(diagnostic));
		assertEquals("", message); //$NON-NLS-1$
	}
}
