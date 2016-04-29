/**
 */
package car.impl;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.MinimalEObjectImpl;

import car.Car;
import car.CarPackage;
import car.Driver;

/**
 * <!-- begin-user-doc --> An implementation of the model object '
 * <em><b>Car</b></em>'. <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link car.impl.CarImpl#getManufacturer <em>Manufacturer</em>}</li>
 * <li>{@link car.impl.CarImpl#getLicensePlate <em>License Plate</em>}</li>
 * <li>{@link car.impl.CarImpl#getSeatCount <em>Seat Count</em>}</li>
 * <li>{@link car.impl.CarImpl#getDriver <em>Driver</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CarImpl extends MinimalEObjectImpl.Container implements Car {
	/**
	 * The default value of the '{@link #getManufacturer()
	 * <em>Manufacturer</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @see #getManufacturer()
	 * @generated
	 * @ordered
	 */
	protected static final String MANUFACTURER_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getManufacturer() <em>Manufacturer</em>}
	 * ' attribute. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getManufacturer()
	 * @generated
	 * @ordered
	 */
	protected String manufacturer = MANUFACTURER_EDEFAULT;

	/**
	 * The default value of the '{@link #getLicensePlate()
	 * <em>License Plate</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @see #getLicensePlate()
	 * @generated
	 * @ordered
	 */
	protected static final String LICENSE_PLATE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getLicensePlate()
	 * <em>License Plate</em>}' attribute. <!-- begin-user-doc --> <!--
	 * end-user-doc -->
	 * 
	 * @see #getLicensePlate()
	 * @generated
	 * @ordered
	 */
	protected String licensePlate = LICENSE_PLATE_EDEFAULT;

	/**
	 * The default value of the '{@link #getSeatCount() <em>Seat Count</em>}'
	 * attribute. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getSeatCount()
	 * @generated
	 * @ordered
	 */
	protected static final int SEAT_COUNT_EDEFAULT = 0;

	/**
	 * The cached value of the '{@link #getSeatCount() <em>Seat Count</em>}'
	 * attribute. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getSeatCount()
	 * @generated
	 * @ordered
	 */
	protected int seatCount = SEAT_COUNT_EDEFAULT;

	/**
	 * The cached value of the '{@link #getDriver() <em>Driver</em>}'
	 * containment reference. <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @see #getDriver()
	 * @generated
	 * @ordered
	 */
	protected Driver driver;

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	protected CarImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CarPackage.Literals.CAR;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public String getManufacturer() {
		return manufacturer;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setManufacturer(String newManufacturer) {
		String oldManufacturer = manufacturer;
		manufacturer = newManufacturer;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					CarPackage.CAR__MANUFACTURER, oldManufacturer, manufacturer));
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public String getLicensePlate() {
		return licensePlate;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setLicensePlate(String newLicensePlate) {
		String oldLicensePlate = licensePlate;
		licensePlate = newLicensePlate;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					CarPackage.CAR__LICENSE_PLATE, oldLicensePlate,
					licensePlate));
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public int getSeatCount() {
		return seatCount;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setSeatCount(int newSeatCount) {
		int oldSeatCount = seatCount;
		seatCount = newSeatCount;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					CarPackage.CAR__SEAT_COUNT, oldSeatCount, seatCount));
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public Driver getDriver() {
		return driver;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	public NotificationChain basicSetDriver(Driver newDriver,
			NotificationChain msgs) {
		Driver oldDriver = driver;
		driver = newDriver;
		if (eNotificationRequired()) {
			ENotificationImpl notification = new ENotificationImpl(this,
					Notification.SET, CarPackage.CAR__DRIVER, oldDriver,
					newDriver);
			if (msgs == null)
				msgs = notification;
			else
				msgs.add(notification);
		}
		return msgs;
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void setDriver(Driver newDriver) {
		if (newDriver != driver) {
			NotificationChain msgs = null;
			if (driver != null)
				msgs = ((InternalEObject) driver).eInverseRemove(this,
						EOPPOSITE_FEATURE_BASE - CarPackage.CAR__DRIVER, null,
						msgs);
			if (newDriver != null)
				msgs = ((InternalEObject) newDriver).eInverseAdd(this,
						EOPPOSITE_FEATURE_BASE - CarPackage.CAR__DRIVER, null,
						msgs);
			msgs = basicSetDriver(newDriver, msgs);
			if (msgs != null)
				msgs.dispatch();
		} else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET,
					CarPackage.CAR__DRIVER, newDriver, newDriver));
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd,
			int featureID, NotificationChain msgs) {
		switch (featureID) {
		case CarPackage.CAR__DRIVER:
			return basicSetDriver(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
		case CarPackage.CAR__MANUFACTURER:
			return getManufacturer();
		case CarPackage.CAR__LICENSE_PLATE:
			return getLicensePlate();
		case CarPackage.CAR__SEAT_COUNT:
			return getSeatCount();
		case CarPackage.CAR__DRIVER:
			return getDriver();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
		case CarPackage.CAR__MANUFACTURER:
			setManufacturer((String) newValue);
			return;
		case CarPackage.CAR__LICENSE_PLATE:
			setLicensePlate((String) newValue);
			return;
		case CarPackage.CAR__SEAT_COUNT:
			setSeatCount((Integer) newValue);
			return;
		case CarPackage.CAR__DRIVER:
			setDriver((Driver) newValue);
			return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
		case CarPackage.CAR__MANUFACTURER:
			setManufacturer(MANUFACTURER_EDEFAULT);
			return;
		case CarPackage.CAR__LICENSE_PLATE:
			setLicensePlate(LICENSE_PLATE_EDEFAULT);
			return;
		case CarPackage.CAR__SEAT_COUNT:
			setSeatCount(SEAT_COUNT_EDEFAULT);
			return;
		case CarPackage.CAR__DRIVER:
			setDriver((Driver) null);
			return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
		case CarPackage.CAR__MANUFACTURER:
			return MANUFACTURER_EDEFAULT == null ? manufacturer != null
					: !MANUFACTURER_EDEFAULT.equals(manufacturer);
		case CarPackage.CAR__LICENSE_PLATE:
			return LICENSE_PLATE_EDEFAULT == null ? licensePlate != null
					: !LICENSE_PLATE_EDEFAULT.equals(licensePlate);
		case CarPackage.CAR__SEAT_COUNT:
			return seatCount != SEAT_COUNT_EDEFAULT;
		case CarPackage.CAR__DRIVER:
			return driver != null;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc --> <!-- end-user-doc -->
	 * 
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (manufacturer: ");
		result.append(manufacturer);
		result.append(", licensePlate: ");
		result.append(licensePlate);
		result.append(", seatCount: ");
		result.append(seatCount);
		result.append(')');
		return result.toString();
	}

} // CarImpl
