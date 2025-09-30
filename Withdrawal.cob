       IDENTIFICATION DIVISION.
       PROGRAM-ID. Withdrawal.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AMOUNT PIC 9(5)V99.
       01 RESULT-BALANCE PIC 9(5)V99.

       LINKAGE SECTION.
       01 ACCOUNT-1-ID  PIC 9(5).
       01 ACCOUNT-1-BALANCE       PIC 9(5)V99.
       01 ERROR-CODE   PIC 9(1).

       PROCEDURE DIVISION USING ACCOUNT-1-ID ACCOUNT-1-BALANCE ERROR-CODE.
           DISPLAY "Enter amount to withdraw: ".
           ACCEPT AMOUNT.      
           DISPLAY "Withdrawing " AMOUNT " from account " ACCOUNT-1-ID.
         
           IF ACCOUNT-1-BALANCE >= AMOUNT
               COMPUTE RESULT-BALANCE = ACCOUNT-1-BALANCE - AMOUNT
               MOVE RESULT-BALANCE TO ACCOUNT-1-BALANCE
               DISPLAY "New balance for account " ACCOUNT-1-ID " : " ACCOUNT-1-BALANCE
               MOVE 0 TO ERROR-CODE
           ELSE
               DISPLAY "Error: Insufficient funds in account " ACCOUNT-1-ID
               MOVE 1 TO ERROR-CODE
           END-IF

           DISPLAY "Withdrawal action completed.".
           GOBACK.
