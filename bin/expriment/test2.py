from Bio import SeqIO

record = SeqIO.read("../debug/gb/D03-T22788-G0177739-1_G0177739-1-SEQ2.ab1", "abi-trim")
print(record)


print(record.letter_annotations)

#record2 = SeqIO.read("../debug/gb/B02-T22788-G0177739-1_G0177739-1-seq1r.seq", "tab")