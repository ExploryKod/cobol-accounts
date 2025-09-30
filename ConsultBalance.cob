       IDENTIFICATION DIVISION.
       PROGRAM-ID. ConsultBalance.
       
       DATA DIVISION.
       LINKAGE SECTION.
       01 ACCOUNT-1-ID  PIC 9(5).
       01 ACCOUNT-1-BALANCE       PIC 9(5)V99.
       01 ERROR-CODE   PIC 9(1).

       PROCEDURE DIVISION USING ACCOUNT-1-ID ACCOUNT-1-BALANCE ERROR-CODE.
           DISPLAY "Consulting balance for account : " ACCOUNT-1-ID.
           DISPLAY "Balance : " ACCOUNT-1-BALANCE.
           MOVE 0 TO ERROR-CODE.
           GOBACK.

           