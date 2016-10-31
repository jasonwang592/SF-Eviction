import matplotlib.pyplot as plt
import pandas as pd
plt.style.use('fivethirtyeight')


def read_file():
	'''Read the eviction file and return the cleaned dataframe and the
	list of eviction reasons'''
	df = pd.read_csv('Eviction_Notices.csv')
	df.columns = ['Eviction ID', 'Address', 'City', 'State',\
		'Zip', 'File Date', 'Non Payment', 'Breach',\
		'Nuisance', 'Illegal Use', 'Failure to Sign Renewal', 'Access Denial',\
		'Unapproved Subtenant', 'Owner Move In', 'Demolition',\
		'Capital Improvement', 'Substantial Rehab', 'Ellis Act Withdrawal',\
		'Condo Conversion', 'Roommate Same Unit', 'Other Cause',\
		'Late Payments', 'Lead Remediation', 'Development',\
		'Good Samaritan Ends', 'Constraints Date', 'Supervisor District',\
		'Neighborhood', 'Location']
	evic_reasons = ['Non_Payment', 'Breach', 'Nuisance', 'Illegal_Use',\
		'Failure_to_Sign_Renewal', 'Access_Denial', 'Unapproved_Subtenant',\
		'Owner_Move_In', 'Demolition', 'Capital_Improvement',\
		'Substantial_Rehab', 'Ellis_Act_Withdrawal', 'Condo_Conversion',\
		'Roommate_Same_Unit', 'Other_Cause', 'Late_Payments',\
		'Lead_Remediation', 'Development', 'Good_Samaritan_Ends']

	'''drop rows with empty neighborhoods. Drop the eviction ID since it doesn't
	provide any info and drop city and state since this is all for San Francisco, CA'''
	df.drop(['Eviction_ID', 'City', 'State'], axis = 1, inplace = True)
	df = df.dropna(subset = ['Neighborhood'])
	df.columns = [c.replace(' ', '_') for c in df.columns]

	return df, evic_reasons

def plot_total_evics(df):
	plt.figure(figsize=(20,10))
	df['Neighborhood'].value_counts().plot(kind = 'barh')
	plt.xlabel('Evictions')
	plt.ylabel('Neighborhood')
	plt.gca().invert_yaxis()
	plt.tight_layout()
	plt.show()

def evic_by_neighborhood_and_reason(df, neighborhoods, evic_reasons):
	'''plots a grouped bar chart for the neighborhoods and eviction reasons
	defined in the arguments of the method'''
	filter_df = df[[col for col in df.columns if col in evic_reasons]]
	filter_df = filter_df[filter_df.index.isin(neighborhoods)]
	filter_df.plot(kind = 'bar', legend = None, figsize = (20,10))
	plt.xlabel('Neighborhood')
	plt.ylabel('Evictions')
	plt.tight_layout()
	plt.show()

def calc_evict_matrix(df, reasons):
	'''returns eviction reason matrix aggregated by Neighborhood to be
	used for further processing and analysis'''
	reasons.append('Neighborhood')
	evict_matrix = df[[col for col in df.columns if col in reasons]]
	group = evict_matrix.groupby('Neighborhood').sum()
	return group

df, evic_reasons = read_file()
mat = calc_evict_matrix(df, evic_reasons)
sn = (mat.index.value_counts()[:10].index.tolist())
evic_by_neighborhood_and_reason(mat, sn, evic_reasons)