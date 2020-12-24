# -*- coding: UTF-8 -*-
from Bio import SeqIO
import re

def feature_extract(filepath):
	record = SeqIO.read(filepath, "genbank")
	for f in record.features:
		for key in f.qualifiers.keys():
			if re.search('gene',key):
				return (f.qualifiers['gene'],f.extract(record.seq))
			elif re.search('shRNA',key,re.I):
				return (f.qualifiers['shRNA'],f.extract(record.seq))



#record = SeqIO.read("../debug/gb/D03-T22788-G0177739-1_G0177739-1-SEQ2.ab1", "abi-trim")
#print(record)
#print(record.letter_annotations)