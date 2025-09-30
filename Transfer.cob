       IDENTIFICATION DIVISION.
       PROGRAM-ID. Transfer.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 AMOUNT PIC 9(5)V99.
       01 RESULT-ACCOUNT-1-BALANCE PIC 9(5)V99.
       01 RESULT-ACCOUNT-2-BALANCE PIC 9(5)V99.

       LINKAGE SECTION.
       01 ACCOUNT-1-ID  PIC 9(5).
       01 ACCOUNT-2-ID  PIC 9(5).

       01 ACCOUNT-1-BALANCE PIC 9(5)V99.
       01 ACCOUNT-2-BALANCE PIC 9(5)V99.
       01 ERROR-CODE         PIC 9(1).

       PROCEDURE DIVISION USING ACCOUNT-1-ID ACCOUNT-2-ID ACCOUNT-1-BALANCE ACCOUNT-2-BALANCE ERROR-CODE.
           DISPLAY "Enter amount to transfer: ".
           ACCEPT AMOUNT.      
           DISPLAY "Transferring " AMOUNT " from account " ACCOUNT-1-ID " to account " ACCOUNT-2-ID.
         
           IF ACCOUNT-1-BALANCE >= AMOUNT
               COMPUTE RESULT-ACCOUNT-1-BALANCE = ACCOUNT-1-BALANCE - AMOUNT
               MOVE RESULT-ACCOUNT-1-BALANCE TO ACCOUNT-1-BALANCE
               DISPLAY "New balance for account " ACCOUNT-1-ID " : " ACCOUNT-1-BALANCE
               COMPUTE RESULT-ACCOUNT-2-BALANCE = ACCOUNT-2-BALANCE + AMOUNT
               MOVE RESULT-ACCOUNT-2-BALANCE TO ACCOUNT-2-BALANCE
               DISPLAY "New balance for account " ACCOUNT-2-ID " : " ACCOUNT-2-BALANCE
           ELSE
               DISPLAY "Error: Insufficient funds in account " ACCOUNT-1-ID
               MOVE 1 TO ERROR-CODE
               GOBACK
           END-IF

           DISPLAY "Transfer action completed.".

           MOVE 0 TO ERROR-CODE.
           GOBACK.
