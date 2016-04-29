/**
 */
package org.eclipse.emf.ecp.view.keyattribute.test.example;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Key Container</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link org.eclipse.emf.ecp.view.keyattribute.test.example.KeyContainer#getKey <em>Key</em>}</li>
 * </ul>
 *
 * @see org.eclipse.emf.ecp.view.keyattribute.test.example.ExamplePackage#getKeyContainer()
 * @model
 * @generated
 */
public interface KeyContainer extends EObject {
	/**
	 * Returns the value of the '<em><b>Key</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Key</em>' attribute isn't clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Key</em>' attribute.
	 * @see #setKey(String)
	 * @see org.eclipse.emf.ecp.view.keyattribute.test.example.ExamplePackage#getKeyContainer_Key()
	 * @model
	 * @generated
	 */
	String getKey();

	/**
	 * Sets the value of the '{@link org.eclipse.emf.ecp.view.keyattribute.test.example.KeyContainer#getKey <em>Key</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Key</em>' attribute.
	 * @see #getKey()
	 * @generated
	 */
	void setKey(String value);

} // KeyContainer
