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

       DATA DIVISION.
       FILE SECTION.
       FD  CITY-TAX-FILE.
       01 CityTax.
         02 LowTax PIC V9999.
         02 HighTax PIC V9999.
         02 CityName PIC X(45).

       WORKING-STORAGE SECTION.
       01 GrossSalary PIC 9(7)V99 VALUE 0.
       01 AdditionalFacilitations PIC 9(2)V9 VALUE 0.
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

       01 LowLevelSalary PIC 9(3)V99 VALUE 700.00.
       01 MidLevelSalary PIC 9(4)V99 VALUE 1300.00.

       01 TaxationBaseInEuro PIC 9(7)V99.
       01 PersonalDeductionInEuro PIC 9(7)V99.
       01 CityLowTaxInEuro PIC 9(7)V99.
       01 CityHighTaxInEuro PIC 9(7)V99.
       01 IncomeTaxInEuro PIC 9(7)V99.
       01 EmployerToPayInEuro PIC 9(7)V99.
       01 NetSalary PIC 9(7)V99 VALUE 0.
       PROCEDURE DIVISION.
         PERFORM ReadAllCities.
         PERFORM ChooseCity.
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
           DISPLAY 'Enter the city number: ' WITH NO ADVANCING
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
                  DISPLAY "You selected: " CityTaxLowTaxPercent"% / " 
                   CityTaxHighTaxPercent"% " CityName
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
       
       GrossToNet.
         DISPLAY "Enter your gross salary: " WITH NO ADVANCING
         ACCEPT GrossSalary 
         COMPUTE SecondPillarInEuro = GrossSalary * SecondPillar
         IF GrossSalary <= LowLevelSalary
      *> up to 700 euros, a fixed amount of 300 euros is deducted from the gross and then multiplied by 15%
           COMPUTE FirstPillarInEuro = (GrossSalary - 300) * FirstPillar
           ADD FirstPillarInEuro, SecondPillarInEuro TO 
           TotalPillarInEuro

           COMPUTE PersonalDeductionInEuro = GrossSalary -
           TotalPillarInEuro
         ELSE
           IF GrossSalary <= MidLevelSalary
             DISPLAY "Mid"
           ELSE
             DISPLAY "High"
           END-IF
         END-IF

         IF PersonalDeductionInEuro <= 0
           MOVE 0 TO TaxationBaseInEuro
           DISPLAY "reaply low"
         ELSE
           DISPLAY "high"
         END-IF

         IF TaxationBaseInEuro < CityTaxBreakingPoint
           COMPUTE CityLowTaxInEuro = TaxationBaseInEuro * 
           SelectedCityLowTax
           MOVE 0 TO CityHighTaxInEuro
           MOVE CityLowTaxInEuro TO IncomeTaxInEuro 
           DISPLAY "reaply low"
         ELSE
           DISPLAY "high"
         END-IF
           
        COMPUTE HealthInsuranceInEuro = GrossSalary *
           HealthInsurancePercent
        COMPUTE EmployerToPayInEuro = GrossSalary + 
           HealthInsuranceInEuro
        COMPUTE NetSalary = TaxationBaseInEuro - IncomeTaxInEuro + 
           personalDeductionInEuro.
        
       DisplayCalculations.
        DISPLAY "Gross " GrossSalary.
        DISPLAY "First pension pillar: " FirstPillarInEuro.
        DISPLAY "Second pension pillar: " SecondPillarInEuro.
        DISPLAY "Personal deduction: " PersonalDeductionInEuro.
        DISPLAY "Taxation base: " TaxationBaseInEuro.
        DISPLAY "Low city tax: " CityLowTaxInEuro.
        DISPLAY "High city tax: " CityHighTaxInEuro.
        DISPLAY "Total taxes: " IncomeTaxInEuro.
        DISPLAY "Net: " NetSalary.
        DISPLAY "----------"
        DISPLAY "Gross " GrossSalary.
        DISPLAY "Health insurance: " HealthInsuranceInEuro.
        DISPLAY "Employers cost: " EmployerToPayInEuro.

