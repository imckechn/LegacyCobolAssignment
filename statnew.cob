      *> Cobal re-engineering for cis 3190
      *> Written by Ian McKechnie (1051662)
      *> Written and completed by Sunday March 28, 2021
       
       identification division.
       
       program-id. stats.
    
      *> File input seciton
       environment division.
       input-output section.
       file-control.
       select input-file assign to dynamic ws-fname organization is line sequential.
       select output-file assign to dynamic ws-out-fname organization is line sequential.

       data division.
       file section.
       fd input-file.
       01 sample-input     pic x(80).
       fd output-file.
       01 output-line      pic x(80).

      *> Variable Decalaration and prime variables with some output that is static for the entire program
       working-storage section.
       77 ws-fname pic x(30).
       77 ws-out-fname pic x(30).
       77 sumOfNumbers   picture s9(14)v9(14) usage is computational-3.
       77 numberCount    picture s9999 usage is computational.
       77 counter    picture s9999 usage is computational.
       77 mean    picture s9(14)v9(14) usage is computational-3.
       77 i    picture s9999 usage is computational.
       77 j    picture s9999 usage is computational.
       77 jp1    picture s9999 usage is computational.
       77 jp2    picture s9999 usage is computational.
       77 standardDeviation  picture s9(14)v9(14) usage is computational-3.
       77 temp picture s9(14)v9(22) usage is computational-3.
       77 variance picture s9(14)v9(14) usage is computational-3.
       77 geoMean picture s9(22)v9(14) usage is computational-3.
       77 harmMean picture s9(14)v9(14) usage is computational-3.
       77 median picture s9(14)v9(14) usage is computational-3.

       01 array-area.
          02 numberArray picture s9(14)v9(14) usage is computational-3
             occurs 1000 times.
       01 input-value.
          02 in-x   picture s9(14)v9(14).
          02 filler picture x(62).
       01 title-line.
          02 filler picture x(1000) value
             '  Mean, Variance, Standard Deviation, Geometric Mean, Harmonic Mean, and Median'.
       01 under-line.
          02 filler picture x(30) value
             '------------------------------'.
       01 col-heads.
          02 filler picture x(21) value '          data values'.
       01 data-line.
          02 filler picture x(5) values spaces.
          02 out-x picture -(14)9.9(4).
      
      *> Print the mean
       01 print-line-1.
          02 filler picture x(30) value ' Mean               =   '.
          02 out-mean picture -(14)9.9(4).
      
      *> Print the Standard Deviation
       01 print-line-2.
          02 filler picture x(30) value ' Standard Deviation = '.
          02 out-standardDeviation picture -(14)9.9(4).

      *> Print the Variance
       01 print-line-3.
          02 filler picture x(30) value ' Variance           = '.
          02 out-variance picture -(14)9.9(4).

      *> Print the Geometric Mean
       01 print-line-4.
          02 filler picture x(30) value ' Geometric Mean     = '.
          02 out-geoMean picture -(14)9.9(4).

      *> Print the Harmonic Mean
       01 print-line-5.
          02 filler picture x(30) value ' Harmonic Mean      = '.
          02 out-harmonicMean picture -(14)9.9(4).

      *> Print the Median
       01 print-line-6.
          02 filler picture x(30) value ' Median             = '.
          02 out-median picture -(14)9.9(4).
 
      *> Loop through the file and get the realivant data
       
       procedure division.
       display "Filename containing book information? ".
       accept ws-fname.

       display "What would you like the output file to be called (include .txt at end)? ".
       accept ws-out-fname.
      *> Write first 4 lines in the file (File header, these are constant)
       open input input-file, output output-file.

      *> Write the header into the file
       write output-line from title-line after advancing 0 lines.   
       write output-line from under-line after advancing 1 lines.   
       write output-line from col-heads after advancing 1 lines.    
       write output-line from under-line after advancing 1 lines.    
       
      *> Set initial values for the numbers being computed
       move 1 to geoMean.
       move zero to sumOfNumbers.
       move zero to standardDeviation.
       move zero to variance.
       move zero to harmMean.
       move zero to median.
       move zero to counter.

      *> Read the file into the array numbers and count the numbers as numberCount
       perform fileExtractionLoop varying numberCount from 1 by 1 until numberCount is greater than 1000 or counter = 1.

      *> Get the mean
       compute numberCount = numberCount - 2.
       compute mean = sumOfNumbers / numberCount.
       
      *> Get the Variance
       perform variance-loop varying i from 1 by 1 until i is greater than numberCount
       compute variance = variance / (numberCount - 1).

      *> Get the Standard Deviation
       compute standardDeviation = variance ** 0.5.
       
      *> Get the Geometric Mean
       perform geometricMean-loop varying i from 1 by 1 until i = numberCount.
       compute temp = 1 / numberCount.
       compute geoMean = (geoMean ** temp) * 10.

      *> Get the Harmonic Mean
       perform harmonicMean-loop varying i from 1 by 1 until i is greater than numberCount.
       compute harmMean = numberCount / harmMean.

      *> Sort the list of numbers then find the median
       perform bubblesort.
       perform median-loop.
           
      *> Print everything to the file 
       perform prints.
       display "Finished".
       
       close input-file, output-file.
       stop run.


       fileExtractionLoop.
       read input-file into input-value at end move 1 to counter.
       if counter = zero then
           move in-x to numberArray(numberCount), out-x
           write output-line from data-line after advancing 1 line
           compute sumOfNumbers = sumOfNumbers + numberArray(numberCount)
       end-if.

       variance-loop.
       compute temp = numberArray(i) - mean.
       compute temp = temp * temp.
       compute variance = variance + temp.
       
       geometricMean-loop.
       compute geoMean = geoMean * numberArray(i).

       harmonicMean-loop.
       compute harmMean = harmMean + (1/numberArray(i)).

      *> The bubble sort algorithm taken from https://craftofcoding.wordpress.com/2021/03/23/coding-cobol-a-bubblesort/
      *> Written by Michael Wirth
      *> Coppied and adapted on March 27th at 3:00pm
       bubblesort.
           perform varying i from 1 by 1 until i is greater than numberCount
              compute jp1 = numberCount - i
              perform varying j from 1 by 1 until j is greater than jp1
                 compute jp2 = j + 1
                 if (numberArray(j) > numberArray(jp2))
                    move numberArray(j) to temp
                    move numberArray(jp2) to numberArray(j)
                    move temp to numberArray(jp2)
                 end-if
              end-perform
           end-perform.

       print-nums.
           move 1 to i.
           perform until i > numberCount
              add 1 to i
           end-perform.
           
       median-loop.
      *> On the next line median is a space filler and it's value given does not matter
      
       divide numberCount by 2 giving median remainder temp.
       compute temp = function mod(numberCount, 2).

       if temp = 0 then
           compute median = ( numberArray(numberCount / 2) + numberArray( (numberCount/2) + 1) ) / 2
       else
           compute median = numberArray((numberCount / 2) + 1)
       end-if.

      *> Here everything is printed to screen
       prints.
       write output-line from under-line after advancing 1 line.
       move mean to out-mean.
       move standardDeviation to out-standardDeviation.
       move variance to out-variance.
       move geoMean to out-geoMean.
       move harmMean to out-harmonicMean.
       move median to out-median.

       write output-line from print-line-1 after advancing 1 line.
       write output-line from print-line-2 after advancing 1 line.
       write output-line from print-line-3 after advancing 1 line.
       write output-line from print-line-4 after advancing 1 line.
       write output-line from print-line-5 after advancing 1 line.
       write output-line from print-line-6 after advancing 1 line.
       write output-line from print-line-6 after advancing 1 line.