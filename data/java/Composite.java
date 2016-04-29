/*******************************************************************************
 * Copyright (c) 2011-2013 EclipseSource Muenchen GmbH and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * EclipseSource Muenchen GmbH - initial API and implementation
 ******************************************************************************/
package org.eclipse.emf.ecp.ui.view.editor.test.model.test;

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Composite</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link org.eclipse.emf.ecp.ui.view.editor.test.model.test.Composite#getComposites <em>Composites</em>}</li>
 * </ul>
 * </p>
 *
 * @see org.eclipse.emf.ecp.ui.view.editor.test.model.test.TestPackage#getComposite()
 * @model
 * @generated
 */
public interface Composite extends Component {
	/**
	 * Returns the value of the '<em><b>Composites</b></em>' containment reference list.
	 * The list contents are of type {@link org.eclipse.emf.ecp.ui.view.editor.test.model.test.Component}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Composites</em>' containment reference isn't clear, there really should be more of a
	 * description here...
	 * </p>
	 * <!-- end-user-doc -->
	 *
	 * @return the value of the '<em>Composites</em>' containment reference list.
	 * @see org.eclipse.emf.ecp.ui.view.editor.test.model.test.TestPackage#getComposite_Composites()
	 * @model containment="true"
	 * @generated
	 */
	EList<Component> getComposites();

} // Composite
