import pandas as pd
#from openpyxl import load_workbook
def file_merge(filelist):
	excels=[pd.read_excel(file) for file in filelist]
	df = pd.concat(excels)
	return df