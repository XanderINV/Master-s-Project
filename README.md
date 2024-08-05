Predictive Modeling of HbA1c Levels in Type 2 Diabetes Using EHR Data
Table of Contents
Project Overview
Features
Methodology
Installation
Usage
Data
Results
Contributors
Acknowledgments
License
Project Overview
This project aims to predict HbA1c levels in patients with Type 2 Diabetes (T2D) using electronic health records (EHR) data. We employed a combination of R and Python to preprocess the data, generate labels, build predictive models, and evaluate their performance. The primary goal is to create models that can assist clinicians in decision-making by providing accurate predictions of HbA1c trends.

Features
Data preprocessing and label generation in R
Predictive modeling and implementation in Python
Comprehensive evaluation metrics
Supplementary materials and code available for reproducibility
Methodology
Tools and Technologies
R version 4.2.1: Used for data preprocessing and label generation
Python 3.12.3: Used for model building and implementation
Visual Studio Code: Integrated development environment
Process
Data Preprocessing: Handled in R, including cleaning, normalization, and label generation.
Model Building: Conducted in Python, involving:
Exploratory Data Analysis (EDA)
Feature engineering
Model selection and tuning
Evaluation
The scripts and relevant libraries are attached in this repository and detailed in the respective code documentation.

Installation
Prerequisites
R (version 4.2.1 or later)
Python (version 3.12.3 or later)
Visual Studio Code (or any preferred IDE)
R Packages
Install the necessary R packages using the following commands:

R
Copy code
install.packages(c("dplyr", "ggplot2", "tidyr", "readr"))
Python Packages
Install the necessary Python packages using the following command:

bash
Copy code
pip install -r requirements.txt
Usage
Clone the repository:

bash
Copy code
git clone https://github.com/yourusername/your-repo-name.git
cd your-repo-name
Run Data Preprocessing and Label Generation (R):
Open the R scripts in Visual Studio Code and execute them sequentially.

Run Model Building and Implementation (Python):
Execute the Python scripts in the appropriate order as indicated in the documentation.

Data
The data used in this project is sourced from [insert data source]. Due to privacy and confidentiality agreements, the raw data is not included in this repository. However, the code to preprocess and analyze the data is provided.

Results
The results of the predictive models, including performance metrics and visualizations, are documented in the results directory. Detailed analysis and discussion can be found in the thesis document attached in this repository.

Contributors
Xander: Primary author and researcher
Dr. Emanuele Trucco: Supervisor and mentor
Chris Sainsbury: Contributor
Atanu Bhattacharjee: Contributor
Acknowledgments
I am deeply grateful to my supervisor, Dr. Emanuele Trucco, for his unwavering support and guidance. Special thanks to Chris Sainsbury and Atanu Bhattacharjee for their invaluable input. Additionally, I acknowledge the role of tools like OpenAI’s ChatGPT in assisting with troubleshooting and editing.

License
This project is licensed under the MIT License - see the LICENSE file for details.
