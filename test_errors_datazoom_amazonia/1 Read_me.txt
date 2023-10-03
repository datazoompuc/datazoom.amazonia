#//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//
# Title: DATAZOOM AMAZONIA - FIND ERRORS
# Data: 2023/10/02
# Programmer: Carolina Moniz De Moura
#//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//,\\'//

This file (\test_errors_datazoom_amazonia) is designed to identify functions within the datazoom.amazonia package that are not functioning correctly.

It consists of several subfiles:
- "test_errors_program.R": contains the code to be executed
- "key_googledrive.json": includes the key required for Google Drive access without human intervention
- "results.xlsx": provides a predefined worksheet structure for exporting results in a more organized format.

To successfully run this code, follow these steps:
1) Ensure that you have all three subfiles mentioned above. To obtain the key for Google Drive access, please refer to the instructions at the end of this document.
   Note: Always make sure to preserve one file named '\test_errors_datazoom_amazonia_virgin' with the same structure so that you can use it as a reference for future code runs.
2) Adjust the working directory in "test_errors_program.R" to match your computer.
   Note: You can easily find and update the necessary sections by searching for the keyword "TO CHANGE IF NEED" (Ctrl+F in English, Ctrl+L in Portuguese).
3) When you begin running the code, please refrain from using your computer and ensure a stable Wi-Fi connection.
   Note: It is advisable to execute the code section by section to minimize potential issues.




To run the code, you'll need to enable the Google Drive API for your project. Here are the steps to do that:

1. **Create a New JSON Key File:**

   If you cannot locate the existing JSON key file, or if it hasn't been created yet, you can follow these steps to create a new one:

   a. **Google Cloud Console:** Go to the [Google Cloud Console](https://console.cloud.google.com/). Make sure you are logged in with the Google account associated with your service account.

   b. **Create a Service Account:** If you haven't already created a service account, follow the steps to create one:
      - Navigate to the "IAM & Admin" section.
      - Select "Service accounts."
      - Click on "Create Service Account" and follow the prompts.

   c. **Generate a JSON Key File:** After creating the service account, you can generate a new JSON key file for it:
      - Click on the newly created service account.
      - Navigate to the "Keys" tab.
      - Click "Add Key" and choose "Create new key."
      - Select JSON as the key type and click "Create."

   d. **Download the JSON Key File:** The JSON key file will be generated and downloaded to your computer. Make sure to save it in the file ~\test_errors_datazoom_amazonia, with the name "key_googledrive.json".

2. **Select Your Project:**

   Make sure you have the correct Google Cloud project selected in the top bar. You should select the project associated with the service account you're using for Google Drive access.

3. **Enable the Google Drive API:**

   To enable the Google Drive API for your project, follow these steps:

   a. Click the navigation menu (☰) in the upper left corner to open the sidebar.

   b. Navigate to "APIs & Services" > "Library."

   c. In the search bar, type "Google Drive API" and select it when it appears in the results.

   d. Click the "Enable" button to enable the API for your project.

4. **Wait for Propagation:**

   After enabling the API, it may take a few minutes for the action to propagate to Google's systems. During this time, some functionality may not work. Wait for a few minutes to ensure the API is fully enabled.

5. **Retry Your R Code:**

   Once the Google Drive API is enabled for your project, you can retry running your R code to access Google Drive data using the `googledrive` package. The error should not occur if the API is enabled correctly.

6. **Check Service Account Permissions:**

   Ensure that your service account associated with the project also has the necessary permissions to access Google Drive. The service account should have been granted access to the specific Google Drive resources you are trying to access.

After following these steps, you should be able to access your Google Drive data without encountering the "Forbidden" error related to the disabled Google Drive API.