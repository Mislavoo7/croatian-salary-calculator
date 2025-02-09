       IDENTIFICATION DIVISION.
       PROGRAM-ID. CROATIAN_SALARY_CALCULATOR.
      *> To compile: cobc -x filename.cob
      *> To run: ./filename
      *>
      *> For module compilation: cobc -m linkedfilename.cob
       AUTHOR. Mislav Kvesić.
       DATE-WRITTEN. 2023-01-19.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT CITY-TAX-FILE
         ASSIGN TO "cityTax.dat"
         ORGANIZATION IS LINE SEQUENTIAL.

         SELECT ALLOWANCES-FILE 
         ASSIGN TO "allowances.dat"
         ORGANIZATION IS LINE SEQUENTIAL.

         SELECT SALARY-FILE ASSIGN TO "salary.txt"
         ORGANIZATION IS LINE SEQUENTIAL.

         SELECT CONFIG-FILE
         ASSIGN TO "config.txt"
         ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CITY-TAX-FILE.
       01 CityTax.
         05 LowTax PIC V9999.
         05 HighTax PIC V9999.
         05 CityName PIC X(45).

       FD  ALLOWANCES-FILE.
       01 Allowances.
         05 AllowanceValue PIC 9V9.
         05 AllowanceLabel PIC X(20).

       FD SALARY-FILE.
       01 PrinLine PIC X(50).
       
       FD CONFIG-FILE.
       01 CONFIG-RECORD.
         05 CONFIG-KEY      PIC X(20).
         05 CONFIG-VALUE    PIC X(20).

       WORKING-STORAGE SECTION.
       01 ConfigIsOpen PIC A(1).
       01 GrossSalary PIC 9(7)V99.
       01 MinSalary PIC 9(4)V99.
       01 CityTaxBreakingPoint PIC 9(4)V99.
       01 CityTaxLowTaxPercent PIC 9(2)V99.
       01 CityTaxHighTaxPercent PIC 9(2)V99.
       01 CityTaxes.
         05 CityTaxTable.
           10 CityTaxEntry OCCURS 565 TIMES INDEXED BY CityIdx.
             15 CityTaxLowTax PIC 9(2)V99.
             15 CityTaxHighTax PIC 9(2)V99.
             15 CityTaxName  PIC X(60).
         05 CityTaxesId           PIC 9(3) VALUE 0.
       01 SelectedLineNumber PIC 9(3).
       01 PensionContributions.
         05 FirstPillar PIC 9V99.
         05 FirstPillarInEuro PIC 9(7)V99.
         05 SecondPillar PIC 9V99.
         05 SecondPillarInEuro PIC 9(7)V99.
         05 TotalPillarInEuro PIC 9(7)V99.
       01 HealthInsurance.
         05 HealthInsurancePercent PIC V999.
         05 HealthInsuranceInEuro PIC 9(7)V99.
       01 SelectedCityName PIC X(45). 
       01 SelectedCityLowTax PIC V9999. 
       01 SelectedCityHighTax PIC V9999. 
       01 RunCityListing PIC X VALUE 'Y'.

       01 AllowancesCalc.
         05 PersonalAllowance PIC 9(2)V9.
         05 TotalAllowances PIC 9(2)V9 VALUE 0.0.
         05 KidsNum PIC 9(2).
         05 KidsAllowance PIC 9(2)V9.
         05 AfterTenthKidValue PIC 9(2)V9.
         05 RunAllowanceListing PIC 99 VALUE 1.
         05 AfterTenthKidAllowance PIC 9V9.
         05 DependentsNum PIC 9(2).
         05 DependentsAllowance PIC 9(2)V9.
         05 TotalDependentsAllowance PIC 9(2)V9.
         05 DisabilityStatus PIC X(1).
         05 SelectedDisability PIC 9(2)V9 VALUE 0.0.
         05 PartialDisabilityAllowance PIC 9V9.
         05 TotalDisabilityAllowance PIC 9V9.
         05 DisabilityAllowance PIC 9(2)V9 VALUE 0.0.
         05 EndOfAllwancesFile PIC X VALUE 'n'.

       01 LowLevelSalary PIC 9(3)V99.
       01 MidLevelSalary PIC 9(4)V99.
       01 MidLevelSalarySplit PIC 9(4)v99.

       01 TaxationBaseInEuro PIC 9(7)V99.
       01 PersonalDeduction PIC 9(7)V99.
       01 Income PIC 9(7)V99.
       01 CityLowTaxInEuro PIC 9(7)V99.
       01 CityHighTaxInEuro PIC 9(7)V99.
       01 IncomeTaxInEuro PIC 9(7)V99.
       01 EmployerToPayInEuro PIC 9(7)V99.
       01 NetSalary PIC 9(7)V99 VALUE 0.
       
       01 NetOrGross PIC X VALUE "1".
          88 BrutToNet VALUE "1". 
          88 NetToBrut VALUE "2". 

       01 Kpn PIC 9(2)V9(16).
       01 Kpv PIC 9(2)V9(16).
       01 LowTaxInPercent PIC 9(2)V99.
       01 HighTaxInPercent PIC 9(2)V99.
       01 ClassCheck PIC 9(7)V99.

       01 MakeReportFile PIC X.
          88 MakeReport VALUE "y".
       01 StayOpen PIC X VALUE "y".

       01 GrossSalaryF  PIC Z(7).99.
       01 FirstPillarInEuroF  PIC Z(7).99.
       01 SecondPillarInEuroF  PIC Z(7).99.
       01 IncomeF  PIC Z(7).99.
       01 TotalAllowancesF  PIC Z(2).99.
       01 PersonalDeductionF  PIC Z(7).99.
       01 TaxationBaseInEuroF  PIC Z(7).99.
       01 CityLowTaxInEuroF  PIC Z(7).99.
       01 CityHighTaxInEuroF  PIC Z(7).99.
       01 IncomeTaxInEuroF  PIC Z(7).99.
       01 HealthInsuranceInEuroF  PIC Z(7).99.
       01 NetSalaryF  PIC Z(7).99.
       01 EmployerToPayInEuroF  PIC Z(7).99.

       PROCEDURE DIVISION.
           PERFORM 1000-MAIN-PROCESS.

       1000-MAIN-PROCESS.
         PERFORM 1100-ReadConfig.
         PERFORM 2000-ReadAllCities.
         PERFORM 2100-ChooseCity.
         PERFORM 2200-ChooseAllowances.
         PERFORM 2300-ReadAllowances.
         PERFORM 2400-ChooseCalculation.
         PERFORM 2500-DisplayCalculations.
         PERFORM 2600-RunReportMaker.
           
         STOP RUN.

       1100-ReadConfig.
         OPEN INPUT CONFIG-FILE.
         PERFORM UNTIL ConfigIsOpen = 'Y'
           READ CONFIG-FILE
             AT END
                 MOVE 'Y' TO ConfigIsOpen
             NOT AT END
               EVALUATE CONFIG-KEY
                 WHEN 'MINSALARY'
                   COMPUTE MinSalary = FUNCTION NUMVAL(CONFIG-VALUE)
                 WHEN 'CITYTAXBREAKINGPOINT'
                   COMPUTE CityTaxBreakingPoint = FUNCTION
                   NUMVAL(CONFIG-VALUE)
                 WHEN 'FIRSTPILLAR'
                   COMPUTE FirstPillar = FUNCTION NUMVAL(CONFIG-VALUE)
                 WHEN 'SECONDPILLAR'
                   COMPUTE SecondPillar = FUNCTION 
                   NUMVAL(CONFIG-VALUE)
                 WHEN 'HEALTHINSURANCE'
                   COMPUTE HealthInsurancePercent = FUNCTION 
                   NUMVAL(CONFIG-VALUE)
                 WHEN 'PERSONALALLOWANCE'
                   COMPUTE PersonalAllowance = FUNCTION
                   NUMVAL(CONFIG-VALUE)
                 WHEN 'LOWLEVELSALARY'
                   COMPUTE LowLevelSalary = FUNCTION
                   NUMVAL(CONFIG-VALUE)
                 WHEN 'MIDLEVELSALARY'
                   COMPUTE MidLevelSalary = FUNCTION
                   NUMVAL(CONFIG-VALUE)
                 WHEN 'MIDLEVELSALARYSPLIT'
                   COMPUTE MidLevelSalarySplit = FUNCTION
                   NUMVAL(CONFIG-VALUE)
               END-EVALUATE
           END-READ
         END-PERFORM.
         DISPLAY "fiiiiiiiiiiiiiiiiiirst" FirstPillar
         CLOSE CONFIG-FILE.


       2000-ReadAllCities.
      *> Present list of all city taxes - there 500+ cities
         DISPLAY 'List of available cities:'
         DISPLAY '-------------------------'
         OPEN INPUT CITY-TAX-FILE
         MOVE 1 TO CityTaxesId

         PERFORM UNTIL RunCityListing = 'N'
          READ CITY-TAX-FILE
            AT END
             MOVE 'N' TO RunCityListing 
            NOT AT END
             MOVE CityName TO CityTaxName(CityTaxesId)
             MOVE LowTax TO CityTaxLowTax(CityTaxesId)
             MOVE HighTax TO CityTaxHighTax(CityTaxesId)

             COMPUTE CityTaxLowTaxPercent = LowTax * 100
             COMPUTE CityTaxHighTaxPercent = HighTax * 100
             DISPLAY CityTaxesId " " CityName " " CityTaxLowTaxPercent 
             "% - " CityTaxHighTaxPercent "%"
             ADD 1 TO CityTaxesId 
            END-READ
         END-PERFORM
        CLOSE CITY-TAX-FILE.

       2100-ChooseCity.
      *> Let user choose his city
         DISPLAY ' '
         DISPLAY 'To see your tax enter the city number: ' 
         WITH NO ADVANCING
         MOVE 'Y' TO RunCityListing
         ACCEPT SelectedLineNumber 
         IF SelectedLineNumber > 0 AND SelectedLineNumber < CityTaxesId
          MOVE CityTaxName(SelectedLineNumber) TO SelectedCityName
          MOVE CityTaxHighTax(SelectedLineNumber) TO SelectedCityHighTax
          MOVE CityTaxLowTax(SelectedLineNumber) TO SelectedCityLowTax

          COMPUTE CityTaxLowTaxPercent = SelectedCityLowTax * 100
          COMPUTE CityTaxHighTaxPercent = SelectedCityHighTax * 100

          DISPLAY " YOU SELECTED: " CityTaxLowTaxPercent"% / " 
          CityTaxHighTaxPercent"% " SelectedCityName
          DISPLAY " -------------------------"
         ELSE
           DISPLAY "Invalid line number. Please try again."
           PERFORM 2100-ChooseCity
         END-If.

       2200-ChooseAllowances.
      *> Let user enter how many kids he has and other allowances
      *> so he can pay less taxes 
         DISPLAY " YOUR ALLOWANCES "
         DISPLAY "How many kids do you have? " WITH NO ADVANCING
         ACCEPT KidsNum.

         DISPLAY "How many dependent persons do you have? " 
         WITH NO ADVANCING
         ACCEPT DependentsNum.
         
         DISPLAY "Are you disabled?" 
         DISPLAY " 'n' for no," 
         DISPLAY " 'p' for partially or" 
         DISPLAY " 't' for total disability"
         DISPLAY "Enter: " WITH NO ADVANCING
         ACCEPT DisabilityStatus.

       2300-ReadAllowances.
      *> Present user his allowances
         OPEN INPUT ALLOWANCES-FILE 
          PERFORM UNTIL EndOfAllwancesFile = 'y'
          READ ALLOWANCES-FILE
           AT END
            MOVE 'y' TO EndOfAllwancesFile
           NOT AT END
      *> each kid has a diff value unless more then 10 kids 
            IF RunAllowanceListing < KidsNum + 1 AND
             RunAllowanceListing < 11
             DISPLAY "- Allowance for kid num " 
             RunAllowanceListing " is             " AllowanceValue
             ADD AllowanceValue TO KidsAllowance
            END-IF
      *> after the tenth kid just get the factor and compute the other
      *kids
         EVALUATE TRUE
           WHEN RunAllowanceListing = 11 AND KidsNum > 10 
             DISPLAY "- Allowance for each other kids is       +" 
             AllowanceValue
             MOVE AllowanceValue TO AfterTenthKidAllowance 
           WHEN RunAllowanceListing = 12
             MOVE AllowanceValue TO DependentsAllowance 
           WHEN RunAllowanceListing = 13
             MOVE AllowanceValue TO PartialDisabilityAllowance 
           WHEN RunAllowanceListing = 14
             MOVE AllowanceValue TO TotalDisabilityAllowance 
           END-EVALUATE

            ADD 1 TO RunAllowanceListing
          END-READ
         END-PERFORM
         CLOSE ALLOWANCES-FILE

         DISPLAY " " 
         DISPLAY "Your personal allowance is               " 
         PersonalAllowance

      *> for each kid after the tenth KidsAllowance is increased by 1.1
         IF KidsNum > 10
           COMPUTE AfterTenthKidValue = (KidsNum - 10) * 
           AfterTenthKidAllowance
           ADD AfterTenthKidValue TO KidsAllowance
         END-IF

         DISPLAY "Total allowance for kids is              " 
         KidsAllowance

      *> for each dependent person add 0.5
         COMPUTE TotalDependentsAllowance = DependentsNum * 
         DependentsAllowance

         DISPLAY "Total allowance for dependent persons is " 
         TotalDependentsAllowance
           
         EVALUATE TRUE
          WHEN DisabilityStatus = "p" 
           MOVE PartialDisabilityAllowance TO DisabilityAllowance
          WHEN DisabilityStatus = "t" 
           MOVE TotalDisabilityAllowance TO DisabilityAllowance
         END-EVALUATE

         DISPLAY "Disability allowance is                  " 
         DisabilityAllowance 

         COMPUTE TotalAllowances = PersonalAllowance + KidsAllowance + 
         TotalDependentsAllowance + DisabilityAllowance
         DISPLAY "Total Allowances is                      " 
         TotalAllowances.

       2400-ChooseCalculation.
      *> Choose brut to net or net to brut
         DISPLAY "-------------------------"
         DISPLAY "WHAT ARE YOU CALCULATING?"
         DISPLAY " Enter 1 if Brut to Net."
         DISPLAY " Enter 2 if Net to Brut."
         DISPLAY "Enter: " WITH NO ADVANCING
         ACCEPT NetOrGross. 
         EVALUATE TRUE
           WHEN BrutToNet 
            PERFORM 2410-GrossToNet
           WHEN NetToBrut
            PERFORM 2420-NetToGross
         END-EVALUATE.
         
       2410-GrossToNet.
      *> When I calcualte Net to brut the GrossSalary won't be zero
         IF GrossSalary = 0
          DISPLAY "Enter your gross salary (use dot, e.g. 1300.05): "
          WITH NO ADVANCING
          ACCEPT GrossSalary 
         END-IF
         COMPUTE SecondPillarInEuro ROUNDED = GrossSalary * SecondPillar
         IF GrossSalary <= LowLevelSalary
      *> hardcoded values are not in the config.txt bc formulas will
      *> change with the values so I have to recomple the code  
      
      *> up to 700 euros, a fixed amount of 300 euros is deducted from the gross and then multiplied by 15%
           COMPUTE FirstPillarInEuro ROUNDED = (GrossSalary - 300) * 
           FirstPillar
           ADD FirstPillarInEuro, SecondPillarInEuro TO 
           TotalPillarInEuro

           COMPUTE Income ROUNDED = GrossSalary -
           TotalPillarInEuro
         ELSE
           COMPUTE PersonalDeduction ROUNDED = 600 * TotalAllowances

           IF GrossSalary <= MidLevelSalary
      *> up to 700 up to 1300 is calculated using the given formula 0.5*(1300-gross). This difference is subtracted from the gross and multiplied by 15%
             COMPUTE FirstPillarInEuro ROUNDED = (GrossSalary - 
             0.5 * (MidLevelSalary - GrossSalary) ) * firstPillar

             ADD FirstPillarInEuro, SecondPillarInEuro TO 
             TotalPillarInEuro

             COMPUTE Income = GrossSalary - 
             TotalPillarInEuro
           ELSE
      *> more then 1300 euro
             COMPUTE FirstPillarInEuro ROUNDED = GrossSalary * 
             FirstPillar

             ADD FirstPillarInEuro, SecondPillarInEuro TO 
             TotalPillarInEuro

             COMPUTE Income = GrossSalary - 
             TotalPillarInEuro
           END-IF
         END-IF

         IF Income <= PersonalDeduction
           MOVE 0 TO TaxationBaseInEuro
         ELSE
           COMPUTE TaxationBaseInEuro = Income - PersonalDeduction 
         END-IF

         IF TaxationBaseInEuro < CityTaxBreakingPoint
           COMPUTE CityLowTaxInEuro ROUNDED = TaxationBaseInEuro * 
           SelectedCityLowTax
           MOVE 0 TO CityHighTaxInEuro
           MOVE CityLowTaxInEuro TO IncomeTaxInEuro 

           COMPUTE NetSalary = Income - IncomeTaxInEuro
         ELSE
           COMPUTE CityLowTaxInEuro ROUNDED = CityTaxBreakingPoint * 
           SelectedCityLowTax 

           COMPUTE CityHighTaxInEuro ROUNDED = (TaxationBaseInEuro - 
           CityTaxBreakingPoint) * SelectedCityHighTax 

           ADD CityLowTaxInEuro, CityHighTaxInEuro TO IncomeTaxInEuro 

           COMPUTE NetSalary = Income - IncomeTaxInEuro
         END-IF
           
        COMPUTE HealthInsuranceInEuro ROUNDED = GrossSalary *
           HealthInsurancePercent
        COMPUTE EmployerToPayInEuro = GrossSalary + 
           HealthInsuranceInEuro.

       2420-NetToGross.
      *> hardcoded values are not in the config.txt bc formulas will
      *> change with the values so I have to recomple the code  
         DISPLAY "Enter your net salary (use dot, e.g. 1300.05): "
         WITH NO ADVANCING
         ACCEPT NetSalary

         COMPUTE PersonalDeduction ROUNDED = 600 * TotalAllowances
         COMPUTE LowTaxInPercent = SelectedCityLowTax * 100
         COMPUTE HighTaxInPercent = SelectedCityHighTax * 100
         COMPUTE Kpn ROuNDED = (LowTaxInPercent / 
         (100 - LowTaxInPercent)) + 1
         COMPUTE Kpv ROUNDED = (HighTaxInPercent / 
         (100 - HighTaxInPercent)) + 1
         COMPUTE ClassCheck ROUNDED = CityTaxBreakingPoint * (1 / Kpn) + 
         PersonalDeduction

         if NetSalary <= PersonalDeduction
      *> Smallest net salary
           MOVE NetSalary TO Income

           PERFORM 2421-IncomeToGross
           DISPLAY "Smallest- " PersonalDeduction
         END-IF

         IF NetSalary > PersonalDeduction AND NetSalary <= ClassCheck 
      *> Middle net salary
           COMPUTE Income ROUNDED = (NetSalary - PersonalDeduction) * 
           Kpn + PersonalDeduction

           PERFORM 2421-IncomeToGross
         ELSE
           IF NetSalary > ClassCheck
      *> High net salary
             COMPUTE Income ROUNDED = CityTaxBreakingPoint + 
             PersonalDeduction + 
             (NetSalary - (CityTaxBreakingPoint - CityTaxBreakingPoint * 
             SelectedCityLowTax + PersonalDeduction)) * Kpv
             PERFORM 2421-IncomeToGross
           END-IF
         END-IF.

       2421-IncomeToGross.
      *> hardcoded values are not in the config.txt bc formulas and
      *> classes will change with the values so I have to recomple the code  
          IF Income <= 285.00
            COMPUTE GrossSalary ROUNDED = Income / 0.95
          END-IF
           
          IF Income > 285.00 AND Income <= 605.00
            COMPUTE GrossSalary ROUNDED = (Income - 45.00) / 0.80
          ELSE
           IF Income > 605.00 AND Income <= 1040.00
             COMPUTE GrossSalary ROUNDED = (Income - 97.50) / 0.725
           ELSE
             COMPUTE GrossSalary ROUNDED = Income / 0.80
           END-IF
          END-IF
      *> At the ent of NetToBrut only Income and GrossSalary are calculated
      *> So run GrossToNet to get all the elements
          PERFORM 2410-GrossToNet.

       2500-DisplayCalculations.
      *> Display all the elements of the calculation
           MOVE GrossSalary TO GrossSalaryF.  
           MOVE FirstPillarInEuro TO FirstPillarInEuroF.  
           MOVE SecondPillarInEuro TO SecondPillarInEuroF.  
           MOVE Income TO IncomeF.  
           MOVE TotalAllowances TO TotalAllowancesF.  
           MOVE PersonalDeduction TO PersonalDeductionF.  
      *> For some reason TaxationBaseInEuroF shows 4 decimmal spacs TODO
           MOVE TaxationBaseInEuro TO TaxationBaseInEuroF.
           MOVE CityLowTaxInEuro TO CityLowTaxInEuroF.  
           MOVE CityHighTaxInEuro TO CityHighTaxInEuroF.  
           MOVE IncomeTaxInEuro TO IncomeTaxInEuroF.  
           MOVE HealthInsuranceInEuro TO HealthInsuranceInEuroF.  
           MOVE NetSalary TO NetSalaryF.  
           MOVE EmployerToPayInEuro TO EmployerToPayInEuroF.  

           DISPLAY " "
           DISPLAY "Salary Calculation Report in €"
           DISPLAY "======================="
           DISPLAY "Gross Salary:          " GrossSalaryF
           DISPLAY "Pension Contributions:"
           DISPLAY "  First Pillar:        " FirstPillarInEuroF
           DISPLAY "  Second Pillar:       " SecondPillarInEuroF
           DISPLAY "Taxable Income:        " IncomeF
           DISPLAY "Allowance  600 * " TotalAllowancesF " " 
           PersonalDeductionF
           DISPLAY "Taxation Base:         " TaxationBaseInEuroF
           DISPLAY "City Taxes:"
           DISPLAY "  Low City Tax:        " CityLowTaxInEuroF
           DISPLAY "  High City Tax:       " CityHighTaxInEuroF
           DISPLAY "Total Income Tax:      " IncomeTaxInEuroF
           DISPLAY "Health Insurance:      " HealthInsuranceInEuroF
           DISPLAY "Net Salary:            " NetSalaryF
           DISPLAY "Employer's Cost:       " EmployerToPayInEuroF
           DISPLAY "=======================".
       
       2600-RunReportMaker.
      *> Ask user if he wants to export the calculation
           DISPLAY "Save it to a report? (y/n) " WITH NO ADVANCING
           ACCEPT MakeReportFile
           IF MakeReportFile = 'y' 
             OPEN OUTPUT SALARY-FILE
             PERFORM 2610-WriteToFile
             CLOSE SALARY-FILE
             DISPLAY "Saved to salary.txt"
           END-IF.

       2610-WriteToFile.
           MOVE "Salary Calculation Report in €" TO PrinLine
           WRITE PrinLine

           MOVE "=================================" TO PrinLine
           WRITE PrinLine
           
           STRING "Gross Salary:          " GrossSalaryF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine.

           MOVE "Pension Contributions:" TO PrinLine
           WRITE PrinLine

           STRING "  First Pillar:        " FirstPillarInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "  Second Pillar:       " SecondPillarInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Taxable Income:        " IncomeF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Allowance  600 * " TotalAllowancesF " " 
           PersonalDeductionF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Taxation Base:       " TaxationBaseInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           MOVE "City Taxes:" TO PrinLine
           WRITE PrinLine

           STRING "  Low City Tax:        " CityLowTaxInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "  High City Tax:       " CityHighTaxInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Total Income Tax:      " IncomeTaxInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Health Insurance:      " HealthInsuranceInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Net Salary:            " NetSalaryF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           STRING "Employer's Cost:       " EmployerToPayInEuroF
           DELIMITED BY SIZE INTO PrinLine
           WRITE PrinLine

           MOVE "=================================" TO PrinLine
           WRITE PrinLine.

