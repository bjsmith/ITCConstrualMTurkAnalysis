#this file manually matches up a few subjects whose data didn't get properly recorded.
#A2FZFUVQ2K1HIO

#this subject wrote to me on email. Can we identify:
#(1) a qualtrics record
which(qualtrics.data$ResponseId=="R_1Cl1k1I10VNHlns")
#this can be identified in "Responses in Progress." So this subject will appear in the data, once the response expires. We should then be able to match to the Qualtrics dataset.
#(2) a turkprime record
which(turkprime.data$AmazonIdentifier=="A2FZFUVQ2K1HIO")
#(3) a PsychoPy record
which(at$qualtricsresponseid=="R_1Cl1k1I10VNHlns")

#Great! NEed to re-check this one. No need to manually match; we just need to wait for the response to get marked as complete.

#ANKF46PES8W3Q
#this subject wrote to me on email. Can we identify:
#(1) a qualtrics record
which(qualtrics.data$ResponseId=="R_1Cl1k1I10VNHlns")
#this can be identified in "Responses in Progress." So this subject will appear in the data, once the response expires. We should then be able to match to the Qualtrics dataset.
#(2) a turkprime record
which(turkprime.data$AmazonIdentifier=="ANKF46PES8W3Q")
#(3) a PsychoPy record
which(at$qualtricsresponseid=="R_1Cl1k1I10VNHlns")



