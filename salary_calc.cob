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

       DATA DIVISION.
       FILE SECTION.
       FD  CITY-TAX-FILE.
       01 CityTax.
         02 LowTax PIC V9999.
         02 HighTax PIC V9999.
         02 CityName PIC X(45).

       FD  ALLOWANCES-FILE.
       01 Allowances.
         02 AllowanceValue PIC 9V9.
         02 AllowanceLabel PIC X(20).

       WORKING-STORAGE SECTION.
       01 GrossSalary PIC 9(7)V99 VALUE 0.
       01 MinSalary PIC 9(4)V99 VALUE 970.00.
       01 CityTaxBreakingPoint PIC 9(4)V99 VALUE 5000.00.
       01 CityTaxId PIC 9(3).
       01 CityTaxLowTaxPercent PIC 9(2)V99.
       01 CityTaxHighTaxPercent PIC 9(2)V99.
       01 SelectedLineNumber PIC 9(3).
       01 PensionContributions.
         02 FirstPillar PIC V99 VALUE 0.15.
         02 FirstPillarInEuro PIC 9(7)V99.
         02 SecondPillar PIC V99 VALUE 0.05.
         02 SecondPillarInEuro PIC 9(7)V99.
         02 TotalPillarInEuro PIC 9(7)V99.
       01 HealthInsurance.
         02 HealthInsurancePercent PIC V999 VALUE 0.165.
         02 HealthInsuranceInEuro PIC 9(7)V99.
       01 SelectedCityName PIC X(45). 
       01 SelectedCityLowTax PIC V9999. 
       01 SelectedCityHighTax PIC V9999. 
       01 RunCityListing PIC X VALUE 'Y'.

       01 AllowancesCalc.
         02 PersonalAllowance PIC 9(2)V9 VALUE 1.0.
         02 TotalAllowances PIC 9(2)V9 VALUE 0.0.
         02 KidsNum PIC 9(2).
         02 KidsAllowance PIC 9(2)V9.
         02 AfterTenthKidValue PIC 9(2)V9.
         02 RunAllowanceListing PIC 99 VALUE 1.
         02 AfterTenthKidAllowance PIC 9V9.
         02 DependentsNum PIC 9(2).
         02 DependentsAllowance PIC 9(2)V9.
         02 TotalDependentsAllowance PIC 9(2)V9.
         02 DisabilityStatus PIC X(1).
         02 SelectedDisability PIC 9(2)V9 VALUE 0.0.
         02 PartialDisabilityAllowance PIC 9V9.
         02 TotalDisabilityAllowance PIC 9V9.
         02 DisabilityAllowance PIC 9(2)V9 VALUE 0.0.
         02 EndOfAllwancesFile PIC X VALUE 'n'.

       01 LowLevelSalary PIC 9(3)V99 VALUE 700.00.
       01 MidLevelSalary PIC 9(4)V99 VALUE 1300.00.
       01 MidLevelSalarySplit PIC 9(4)v99 VALUE 1022.00.

       01 TaxationBaseInEuro PIC 9(7)V99.
       01 PersonalDeduction PIC 9(7)V99.
       01 Income PIC 9(7)V99.
       01 CityLowTaxInEuro PIC 9(7)V99.
       01 CityHighTaxInEuro PIC 9(7)V99.
       01 IncomeTaxInEuro PIC 9(7)V99.
       01 EmployerToPayInEuro PIC 9(7)V99.
       01 NetSalary PIC 9(7)V99 VALUE 0.
       PROCEDURE DIVISION.
         PERFORM ReadAllCities.
         PERFORM ChooseCity.
         PERFORM ChooseAllowances.
         PERFORM ReadAllowances.
         PERFORM GrossToNet.
         PERFORM DisplayCalculations.

           
         STOP RUN.

       ReadAllCities.
         DISPLAY 'List of available cities:'
         DISPLAY '-------------------------'
         OPEN INPUT CITY-TAX-FILE
         MOVE 1 TO CityTaxId
         PERFORM UNTIL RunCityListing = 'N'
          READ CITY-TAX-FILE
            AT END
             MOVE 'N' TO RunCityListing 
            NOT AT END
             COMPUTE CityTaxLowTaxPercent = LowTax * 100
             COMPUTE CityTaxHighTaxPercent = HighTax * 100
             DISPLAY CityTaxId " " CityName " " CityTaxLowTaxPercent 
             "% - " CityTaxHighTaxPercent "%"
             ADD 1 TO CityTaxId
            END-READ
         END-PERFORM
        CLOSE CITY-TAX-FILE.

       ChooseCity.
           DISPLAY ' '
           DISPLAY 'To see your tax enter the city number: ' 
           WITH NO ADVANCING
           MOVE 'Y' TO RunCityListing
           ACCEPT SelectedLineNumber 
           IF SelectedLineNumber > 0 AND SelectedLineNumber < CityTaxId
            MOVE 1 TO CityTaxId
            OPEN INPUT CITY-TAX-FILE
            PERFORM UNTIL RunCityListing = 'N'
             READ CITY-TAX-FILE
              AT END
                MOVE 'N' TO RunCityListing
              NOT AT END
                IF CityTaxId = SelectedLineNumber
                  MOVE LowTax TO SelectedCityLowTax
                  MOVE HighTax TO SelectedCityHighTax
                  MOVE CityName TO SelectedCityName
                  COMPUTE CityTaxLowTaxPercent = LowTax * 100
                  COMPUTE CityTaxHighTaxPercent = HighTax * 100
                  DISPLAY " YOU SELECTED: " CityTaxLowTaxPercent"% / " 
                   CityTaxHighTaxPercent"% " CityName
                  DISPLAY " -------------------------"
                  MOVE 'N' TO RunCityListing
                END-IF
                ADD 1 TO CityTaxId
             END-READ
            END-PERFORM
            CLOSE CITY-TAX-FILE
           ELSE
             DISPLAY "Invalid line number. Please try again."
             PERFORM ChooseCity
           END-If.

       ChooseAllowances.
         DISPLAY " YOUR ALLOWANCES "
         DISPLAY "How many kids do you have? " WITH NO ADVANCING
         ACCEPT KidsNum.

         DISPLAY "How many dependent persons do you have? " 
         WITH NO ADVANCING
         ACCEPT DependentsNum.
         
         DISPLAY "Are you disabled?" 
         DISPLAY "('n' for no, 'p' for partially, 't' for total)? "
         WITH NO ADVANCING.
         ACCEPT DisabilityStatus.

       ReadAllowances.
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
            IF RunAllowanceListing = 11 AND KidsNum > 10 
             DISPLAY "- Allowance for each other kids is       +" 
             AllowanceValue
             MOVE AllowanceValue TO AfterTenthKidAllowance 
            END-IF 

            IF RunAllowanceListing = 12
             MOVE AllowanceValue TO DependentsAllowance 
            END-IF 

            IF RunAllowanceListing = 13
             MOVE AllowanceValue TO PartialDisabilityAllowance 
            END-IF 

            IF RunAllowanceListing = 14
             MOVE AllowanceValue TO TotalDisabilityAllowance 
            END-IF 

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
           
         IF DisabilityStatus = "p" 
           MOVE PartialDisabilityAllowance TO DisabilityAllowance
         END-IF

         IF DisabilityStatus = "t" 
           MOVE TotalDisabilityAllowance TO DisabilityAllowance
         END-IF

         DISPLAY "Disability allowance is                  " 
         DisabilityAllowance 

         COMPUTE TotalAllowances = PersonalAllowance + KidsAllowance + 
         TotalDependentsAllowance + DisabilityAllowance
         DISPLAY "Total Allowances is                      " 
         TotalAllowances.
         
       GrossToNet.
         DISPLAY "Enter your gross salary: " WITH NO ADVANCING
         ACCEPT GrossSalary 
         COMPUTE SecondPillarInEuro = GrossSalary * SecondPillar
         IF GrossSalary <= LowLevelSalary
      *> up to 700 euros, a fixed amount of 300 euros is deducted from the gross and then multiplied by 15%
           COMPUTE FirstPillarInEuro = (GrossSalary - 300) * FirstPillar
           ADD FirstPillarInEuro, SecondPillarInEuro TO 
           TotalPillarInEuro

           COMPUTE Income = GrossSalary -
           TotalPillarInEuro
         ELSE
           COMPUTE PersonalDeduction = 600 * TotalAllowances

           IF GrossSalary <= MidLevelSalary
      *> up to 700 up to 1300 is calculated using the given formula 0.5*(1300-gross). This difference is subtracted from the gross and multiplied by 15%
             COMPUTE FirstPillarInEuro = (GrossSalary - 
             0.5 * (MidLevelSalary - GrossSalary) ) * firstPillar

             ADD FirstPillarInEuro, SecondPillarInEuro TO 
             TotalPillarInEuro

             COMPUTE Income = GrossSalary - 
             TotalPillarInEuro
           ELSE
      *> more then 1300 euro
             COMPUTE FirstPillarInEuro = GrossSalary * FirstPillar

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
           COMPUTE CityLowTaxInEuro = TaxationBaseInEuro * 
           SelectedCityLowTax
           MOVE 0 TO CityHighTaxInEuro
           MOVE CityLowTaxInEuro TO IncomeTaxInEuro 

           COMPUTE NetSalary = Income - IncomeTaxInEuro
         ELSE
           COMPUTE CityLowTaxInEuro = CityTaxBreakingPoint * 
           SelectedCityLowTax 

           COMPUTE CityHighTaxInEuro = (TaxationBaseInEuro - 
           CityTaxBreakingPoint) * SelectedCityHighTax 

           ADD CityLowTaxInEuro, CityHighTaxInEuro TO IncomeTaxInEuro 

           COMPUTE NetSalary = Income - IncomeTaxInEuro
         END-IF
           
        COMPUTE HealthInsuranceInEuro = GrossSalary *
           HealthInsurancePercent
        COMPUTE EmployerToPayInEuro = GrossSalary + 
           HealthInsuranceInEuro.
        
       DisplayCalculations.
        DISPLAY "Gross " GrossSalary.
        DISPLAY "First pension pillar: " FirstPillarInEuro.
        DISPLAY "Second pension pillar: " SecondPillarInEuro.
        DISPLAY "Income: " Income.
        DISPLAY "Taxation base: " TaxationBaseInEuro.
        DISPLAY "Low city tax: " CityLowTaxInEuro.
        DISPLAY "High city tax: " CityHighTaxInEuro.
        DISPLAY "Total taxes: " IncomeTaxInEuro.
        DISPLAY "Net: " NetSalary.
        DISPLAY "----------"
        DISPLAY "Gross " GrossSalary.
        DISPLAY "Health insurance: " HealthInsuranceInEuro.
        DISPLAY "Employers cost: " EmployerToPayInEuro.

