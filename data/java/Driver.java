/**
 */
package car;

/**
 * <!-- begin-user-doc --> A representation of the model object '
 * <em><b>Driver</b></em>'. <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link car.Driver#getAge <em>Age</em>}</li>
 * </ul>
 * </p>
 *
 * @see car.CarPackage#getDriver()
 * @model
 * @generated
 */
public interface Driver extends Person {
	/**
	 * Returns the value of the '<em><b>Age</b></em>' attribute. <!--
	 * begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Age</em>' attribute isn't clear, there really
	 * should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Age</em>' attribute.
	 * @see #setAge(int)
	 * @see car.CarPackage#getDriver_Age()
	 * @model
	 * @generated
	 */
	int getAge();

	/**
	 * Sets the value of the '{@link car.Driver#getAge <em>Age</em>}' attribute.
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @param value
	 *            the new value of the '<em>Age</em>' attribute.
	 * @see #getAge()
	 * @generated
	 */
	void setAge(int value);

} // Driver
