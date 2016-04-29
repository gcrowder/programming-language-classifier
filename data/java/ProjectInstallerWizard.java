/**
 *
 */
package org.eclipse.emf.ecp.ide.view.service.test;

import java.util.ArrayList;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.ui.CommonUIPlugin;
import org.eclipse.emf.common.ui.wizard.AbstractExampleInstallerWizard;
import org.eclipse.emf.common.ui.wizard.ExampleInstallerWizard;
import org.eclipse.emf.common.util.URI;

/**
 * @author Alexandra Buzila
 *
 */
public class ProjectInstallerWizard extends ExampleInstallerWizard {

	private final String projectName = "TestIDEViewRegistryProjectResources";

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.ui.wizard.AbstractExampleInstallerWizard#installExample(org.eclipse.core.runtime.
	 * IProgressMonitor)
	 */
	@Override
	protected void installExample(IProgressMonitor progressMonitor)
		throws Exception {
		super.installExample(progressMonitor);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.ui.wizard.ExampleInstallerWizard#
	 * loadFromExtensionPoints()
	 */
	@Override
	protected void loadFromExtensionPoints() {

		projectDescriptors = new ArrayList<ProjectDescriptor>();
		filesToOpen = new ArrayList<FileToOpen>();
		final String wizardID = "org.eclipse.emf.ecp.ide.view.service.test.ProjectInstallerWizard"; //$NON-NLS-1$

		final IExtensionPoint extensionPoint = Platform.getExtensionRegistry()
			.getExtensionPoint(CommonUIPlugin.INSTANCE.getSymbolicName(), "examples"); //$NON-NLS-1$
		final IConfigurationElement[] exampleElements = extensionPoint
			.getConfigurationElements();
		for (int i = 0; i < exampleElements.length; i++) {
			final IConfigurationElement exampleElement = exampleElements[i];
			if ("example".equals(exampleElement.getName()) //$NON-NLS-1$
				&& wizardID.equals(exampleElement.getAttribute("wizardID"))) { //$NON-NLS-1$
				final IConfigurationElement[] projectDescriptorElements = exampleElement
					.getChildren("projectDescriptor"); //$NON-NLS-1$
				for (int j = 0; j < projectDescriptorElements.length; j++) {

					final IConfigurationElement projectDescriptorElement = projectDescriptorElements[j];
					final String contentURI = projectDescriptorElement
						.getAttribute("contentURI"); //$NON-NLS-1$
					if (projectName != null && contentURI != null) {
						final AbstractExampleInstallerWizard.ProjectDescriptor projectDescriptor = new AbstractExampleInstallerWizard.ProjectDescriptor();
						projectDescriptor.setName(projectName);

						URI uri = URI.createURI(contentURI);
						if (uri.isRelative()) {
							uri = URI
								.createPlatformPluginURI(
									projectDescriptorElement
										.getContributor().getName()
										+ "/" + contentURI, true); //$NON-NLS-1$
						}
						projectDescriptor.setContentURI(uri);
						projectDescriptor
							.setDescription(projectDescriptorElement
								.getAttribute("description")); //$NON-NLS-1$
						projectDescriptors.add(projectDescriptor);
					}
				}

				if (!projectDescriptors.isEmpty()) {
					final IConfigurationElement[] openElements = exampleElement
						.getChildren("fileToOpen"); //$NON-NLS-1$
					for (int j = 0; j < openElements.length; j++) {
						final IConfigurationElement openElement = openElements[j];
						final String location = openElement
							.getAttribute("location"); //$NON-NLS-1$
						if (location != null) {
							final AbstractExampleInstallerWizard.FileToOpen fileToOpen = new AbstractExampleInstallerWizard.FileToOpen();
							fileToOpen.setLocation(location);
							fileToOpen.setEditorID(openElement
								.getAttribute("editorID")); //$NON-NLS-1$
							filesToOpen.add(fileToOpen);
						}
					}

					// Only one example per wizard
					break;
				}
			}
		}
	}
}
