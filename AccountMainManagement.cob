       IDENTIFICATION DIVISION.
       PROGRAM-ID. AccountMainManagement.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACCOUNT-1-ID         PIC 9(5) VALUE 23.
       01 ACCOUNT-2-ID         PIC 9(5) VALUE 45.
       01 ACCOUNT-1-BALANCE    PIC 9(5)V99 VALUE 2000.00.
       01 ACCOUNT-2-BALANCE    PIC 9(5)V99 VALUE 7000.00.
       01 USER-CHOICE          PIC X(1).
       01 ERROR-CODE           PIC 9(1).
       01 END-PROG             PIC 9(1) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOOP.
           DISPLAY "Menu:".
           DISPLAY "1. Consult Balance".
           DISPLAY "2. Virement du compte n°" ACCOUNT-1-ID " au compte n°" ACCOUNT-2-ID.
           DISPLAY "3. Withdrawal".
           DISPLAY "4. Quitter".
           ACCEPT USER-CHOICE.
           EVALUATE USER-CHOICE
               WHEN "1"
                   CALL "ConsultBalance" USING ACCOUNT-1-ID ACCOUNT-1-BALANCE ERROR-CODE
                   DISPLAY "Exiting program..."
               WHEN "2"
                   CALL "Transfer" USING ACCOUNT-1-ID ACCOUNT-2-ID ACCOUNT-1-BALANCE ACCOUNT-2-BALANCE ERROR-CODE
                   DISPLAY "Exiting program..."
               WHEN "3"
                   CALL "Withdrawal" USING ACCOUNT-1-ID ACCOUNT-1-BALANCE ERROR-CODE
                   DISPLAY "Exiting program..."
               WHEN "4"
                   MOVE 1 TO END-PROG
               WHEN OTHER
                   DISPLAY "Invalid choice."
           END-EVALUATE.
           IF END-PROG NOT = 1
               GO TO MAIN-LOOP
           END-IF.
       DISPLAY "Exit code " ERROR-CODE.
       STOP RUN.



           