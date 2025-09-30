       IDENTIFICATION DIVISION.
       PROGRAM-ID. WebBanking.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ACCOUNT-1-ID         PIC 9(5) VALUE 23.
       01 ACCOUNT-2-ID         PIC 9(5) VALUE 45.
       01 ACCOUNT-1-BALANCE    PIC 9(5)V99 VALUE 2000.00.
       01 ACCOUNT-2-BALANCE    PIC 9(5)V99 VALUE 7000.00.
       01 ERROR-CODE           PIC 9(1).
       01 ACTION               PIC X(20).
       01 AMOUNT               PIC 9(5)V99.
       01 FROM-ACCOUNT         PIC 9(5).
       01 TO-ACCOUNT           PIC 9(5).
       01 RESULT-BALANCE       PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Content-Type: text/html; charset=UTF-8"
           DISPLAY ""
           
           MOVE "GET" TO ACTION
           
           EVALUATE ACTION
               WHEN "GET"
                   PERFORM DISPLAY-MAIN-PAGE
               WHEN "POST"
                   PERFORM PROCESS-FORM
               WHEN OTHER
                   PERFORM DISPLAY-MAIN-PAGE
           END-EVALUATE.
           
           STOP RUN.

       DISPLAY-MAIN-PAGE.
           DISPLAY "<!DOCTYPE html>"
           DISPLAY "<html lang='fr'>"
           DISPLAY "<head>"
           DISPLAY "    <meta charset='UTF-8'>"
           DISPLAY "    <meta name='viewport' content='width=device-width, initial-scale=1.0'>"
           DISPLAY "    <title>Systeme Bancaire COBOL</title>"
           DISPLAY "    <style>"
           DISPLAY "        * { margin: 0; padding: 0; box-sizing: border-box; }"
           DISPLAY "        body { font-family: Arial, sans-serif; background: #333; min-height: 100vh; display: flex; align-items: center; justify-content: center; padding: 20px; }"
           DISPLAY "        .container { background: #333; border-radius: 20px; padding: 40px; max-width: 800px; width: 100%; text-align: center; box-shadow: 0 20px 40px rgba(0, 0, 0, 0.3); }"
           DISPLAY "        h1 { color: #e8e8e8; font-size: 2.5rem; margin-bottom: 20px; font-weight: 700; }"
           DISPLAY "        .subtitle { color: #b0b0b0; font-size: 1.2rem; margin-bottom: 40px; }"
           DISPLAY "        .cobol-badge { display: inline-block; background: linear-gradient(45deg, #4a9eff, #0066cc); color: white; padding: 8px 16px; border-radius: 25px; font-size: 0.9rem; font-weight: 600; margin-bottom: 30px; text-transform: uppercase; }"
           DISPLAY "        .accounts { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 40px; }"
           DISPLAY "        .account-card { background: #404040; border-radius: 15px; padding: 20px; border: 1px solid #555; }"
           DISPLAY "        .account-id { color: #4a9eff; font-size: 1.1rem; font-weight: 600; margin-bottom: 10px; }"
           DISPLAY "        .account-balance { color: #e8e8e8; font-size: 1.5rem; font-weight: 700; }"
           DISPLAY "        .actions { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 15px; margin-bottom: 30px; }"
           DISPLAY "        .btn { background: linear-gradient(45deg, #4a9eff, #0066cc); color: white; border: none; padding: 15px 25px; border-radius: 10px; font-size: 1rem; font-weight: 600; cursor: pointer; transition: all 0.3s ease; text-decoration: none; display: inline-block; }"
           DISPLAY "        .btn:hover { transform: translateY(-2px); box-shadow: 0 10px 20px rgba(74, 158, 255, 0.3); }"
           DISPLAY "        .form-container { background: #404040; border-radius: 15px; padding: 30px; margin-top: 20px; }"
           DISPLAY "        .form-group { margin-bottom: 20px; text-align: left; }"
           DISPLAY "        .form-group label { color: #e8e8e8; display: block; margin-bottom: 5px; font-weight: 600; }"
           DISPLAY "        .form-group input, .form-group select { width: 100%; padding: 12px; border: 1px solid #555; border-radius: 8px; background: #333; color: #e8e8e8; font-size: 1rem; }"
           DISPLAY "        .form-group input:focus, .form-group select:focus { outline: none; border-color: #4a9eff; box-shadow: 0 0 10px rgba(74, 158, 255, 0.3); }"
           DISPLAY "        .btn-group { display: flex; gap: 10px; justify-content: center; margin-top: 20px; }"
           DISPLAY "        .btn-secondary { background: #666; }"
           DISPLAY "        .btn-secondary:hover { background: #777; }"
           DISPLAY "        @media (max-width: 768px) { .container { padding: 20px; } h1 { font-size: 2rem; } .actions { grid-template-columns: 1fr; } }"
           DISPLAY "    </style>"
           DISPLAY "</head>"
           DISPLAY "<body>"
           DISPLAY "    <div class='container'>"
           DISPLAY "        <h1>Systeme Bancaire COBOL</h1>"
           DISPLAY "        <div class='cobol-badge'>Powered by COBOL</div>"
           DISPLAY "        <p class='subtitle'>Interface Web Moderne</p>"
           
           DISPLAY "        <div class='accounts'>"
           DISPLAY "            <div class='account-card'>"
           DISPLAY "                <div class='account-id'>Compte " ACCOUNT-1-ID "</div>"
           DISPLAY "                <div class='account-balance'>" ACCOUNT-1-BALANCE " EUR</div>"
           DISPLAY "            </div>"
           DISPLAY "            <div class='account-card'>"
           DISPLAY "                <div class='account-id'>Compte " ACCOUNT-2-ID "</div>"
           DISPLAY "                <div class='account-balance'>" ACCOUNT-2-BALANCE " EUR</div>"
           DISPLAY "            </div>"
           DISPLAY "        </div>"
           
           DISPLAY "        <div class='actions'>"
           DISPLAY "            <a href='WebBanking.cgi?action=consult' class='btn'>Consulter</a>"
           DISPLAY "            <a href='WebBanking.cgi?action=transfer' class='btn'>Virement</a>"
           DISPLAY "            <a href='WebBanking.cgi?action=withdraw' class='btn'>Retrait</a>"
           DISPLAY "        </div>"
           
           DISPLAY "    </div>"
           DISPLAY "</body>"
           DISPLAY "</html>".

       PROCESS-FORM.
           MOVE "GET" TO ACTION
           
           EVALUATE ACTION
               WHEN "action=consult"
                   PERFORM PROCESS-CONSULT
               WHEN "action=transfer"
                   PERFORM PROCESS-TRANSFER
               WHEN "action=withdraw"
                   PERFORM PROCESS-WITHDRAW
               WHEN OTHER
                   PERFORM DISPLAY-MAIN-PAGE
           END-EVALUATE.

       PROCESS-CONSULT.
           DISPLAY "<!DOCTYPE html>"
           DISPLAY "<html lang='fr'>"
           DISPLAY "<head><meta charset='UTF-8'><title>Resultat</title></head>"
           DISPLAY "<body style='font-family: Arial; background: #333; color: #e8e8e8; padding: 20px;'>"
           DISPLAY "<div style='max-width: 600px; margin: 0 auto; background: #404040; padding: 30px; border-radius: 15px;'>"
           DISPLAY "<h2>Consultation du solde</h2>"
           DISPLAY "<p>Compte " ACCOUNT-1-ID " : " ACCOUNT-1-BALANCE " EUR</p>"
           DISPLAY "<p>Compte " ACCOUNT-2-ID " : " ACCOUNT-2-BALANCE " EUR</p>"
           DISPLAY "<a href='WebBanking.cgi' style='color: #4a9eff; text-decoration: none;'>Retour au menu</a>"
           DISPLAY "</div></body></html>".

       PROCESS-TRANSFER.
           DISPLAY "<!DOCTYPE html>"
           DISPLAY "<html lang='fr'>"
           DISPLAY "<head><meta charset='UTF-8'><title>Resultat</title></head>"
           DISPLAY "<body style='font-family: Arial; background: #333; color: #e8e8e8; padding: 20px;'>"
           DISPLAY "<div style='max-width: 600px; margin: 0 auto; background: #404040; padding: 30px; border-radius: 15px;'>"
           DISPLAY "<h2>Virement effectue</h2>"
           DISPLAY "<p>Virement en cours de traitement...</p>"
           DISPLAY "<a href='WebBanking.cgi' style='color: #4a9eff; text-decoration: none;'>Retour au menu</a>"
           DISPLAY "</div></body></html>".

       PROCESS-WITHDRAW.
           DISPLAY "<!DOCTYPE html>"
           DISPLAY "<html lang='fr'>"
           DISPLAY "<head><meta charset='UTF-8'><title>Resultat</title></head>"
           DISPLAY "<body style='font-family: Arial; background: #333; color: #e8e8e8; padding: 20px;'>"
           DISPLAY "<div style='max-width: 600px; margin: 0 auto; background: #404040; padding: 30px; border-radius: 15px;'>"
           DISPLAY "<h2>Retrait effectue</h2>"
           DISPLAY "<p>Retrait en cours de traitement...</p>"
           DISPLAY "<a href='WebBanking.cgi' style='color: #4a9eff; text-decoration: none;'>Retour au menu</a>"
           DISPLAY "</div></body></html>".