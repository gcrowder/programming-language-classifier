/**
 */
package car;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc --> A representation of the model object '
 * <em><b>Car</b></em>'. <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link car.Car#getManufacturer <em>Manufacturer</em>}</li>
 * <li>{@link car.Car#getLicensePlate <em>License Plate</em>}</li>
 * <li>{@link car.Car#getSeatCount <em>Seat Count</em>}</li>
 * <li>{@link car.Car#getDriver <em>Driver</em>}</li>
 * </ul>
 * </p>
 *
 * @see car.CarPackage#getCar()
 * @model
 * @generated
 */
public interface Car extends EObject {
	/**
	 * Returns the value of the '<em><b>Manufacturer</b></em>' attribute. <!--
	 * begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Manufacturer</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Manufacturer</em>' attribute.
	 * @see #setManufacturer(String)
	 * @see car.CarPackage#getCar_Manufacturer()
	 * @model
	 * @generated
	 */
	String getManufacturer();

	/**
	 * Sets the value of the '{@link car.Car#getManufacturer
	 * <em>Manufacturer</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @param value
	 *            the new value of the '<em>Manufacturer</em>' attribute.
	 * @see #getManufacturer()
	 * @generated
	 */
	void setManufacturer(String value);

	/**
	 * Returns the value of the '<em><b>License Plate</b></em>' attribute. <!--
	 * begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>License Plate</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>License Plate</em>' attribute.
	 * @see #setLicensePlate(String)
	 * @see car.CarPackage#getCar_LicensePlate()
	 * @model
	 * @generated
	 */
	String getLicensePlate();

	/**
	 * Sets the value of the '{@link car.Car#getLicensePlate
	 * <em>License Plate</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @param value
	 *            the new value of the '<em>License Plate</em>' attribute.
	 * @see #getLicensePlate()
	 * @generated
	 */
	void setLicensePlate(String value);

	/**
	 * Returns the value of the '<em><b>Seat Count</b></em>' attribute. <!--
	 * begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Seat Count</em>' attribute isn't clear, there
	 * really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Seat Count</em>' attribute.
	 * @see #setSeatCount(int)
	 * @see car.CarPackage#getCar_SeatCount()
	 * @model
	 * @generated
	 */
	int getSeatCount();

	/**
	 * Sets the value of the '{@link car.Car#getSeatCount <em>Seat Count</em>}'
	 * attribute. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @param value
	 *            the new value of the '<em>Seat Count</em>' attribute.
	 * @see #getSeatCount()
	 * @generated
	 */
	void setSeatCount(int value);

	/**
	 * Returns the value of the '<em><b>Driver</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Driver</em>' containment reference isn't
	 * clear, there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * 
	 * @return the value of the '<em>Driver</em>' containment reference.
	 * @see #setDriver(Driver)
	 * @see car.CarPackage#getCar_Driver()
	 * @model containment="true"
	 * @generated
	 */
	Driver getDriver();

	/**
	 * Sets the value of the '{@link car.Car#getDriver <em>Driver</em>}'
	 * containment reference. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @param value
	 *            the new value of the '<em>Driver</em>' containment reference.
	 * @see #getDriver()
	 * @generated
	 */
	void setDriver(Driver value);

} // Car
