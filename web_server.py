#!/usr/bin/env python3
"""
Serveur web simple pour tester l'interface bancaire COBOL
"""

import http.server
import socketserver
import subprocess
import os
import sys
from urllib.parse import urlparse, parse_qs

class COBOLCGIHandler(http.server.CGIHTTPRequestHandler):
    """Handler personnalisé pour exécuter les programmes COBOL CGI"""
    
    def do_GET(self):
        """Gestion des requêtes GET"""
        if self.path == '/' or self.path == '/WebBanking.cgi':
            self.run_cobol_cgi()
        else:
            super().do_GET()
    
    def do_POST(self):
        """Gestion des requêtes POST"""
        if self.path == '/WebBanking.cgi':
            self.run_cobol_cgi()
        else:
            super().do_POST()
    
    def run_cobol_cgi(self):
        """Exécute le programme COBOL CGI"""
        try:
            # Compiler le programme COBOL s'il n'existe pas
            if not os.path.exists('WebBanking'):
                print("🔨 Compilation du programme COBOL CGI...")
                result = subprocess.run(['cobc', '-free', '-x', 'WebBanking.cob'], 
                                      capture_output=True, text=True)
                if result.returncode != 0:
                    self.send_error(500, f"Erreur de compilation COBOL: {result.stderr}")
                    return
            
            # Exécuter le programme COBOL
            env = os.environ.copy()
            env['REQUEST_METHOD'] = self.command
            env['QUERY_STRING'] = self.path.split('?')[1] if '?' in self.path else ''
            
            # Pour POST, lire les données du formulaire
            if self.command == 'POST':
                content_length = int(self.headers.get('Content-Length', 0))
                post_data = self.rfile.read(content_length).decode('utf-8')
                env['QUERY_STRING'] = post_data
            
            result = subprocess.run(['./WebBanking'], 
                                  env=env, 
                                  capture_output=True, 
                                  text=True)
            
            if result.returncode == 0:
                # Envoyer la réponse HTML
                self.send_response(200)
                self.send_header('Content-type', 'text/html; charset=utf-8')
                self.end_headers()
                self.wfile.write(result.stdout.encode('utf-8'))
            else:
                self.send_error(500, f"Erreur d'exécution COBOL: {result.stderr}")
                
        except Exception as e:
            self.send_error(500, f"Erreur: {str(e)}")

def main():
    """Fonction principale du serveur"""
    PORT = 9001
    
    # Vérifier si GnuCOBOL est installé
    try:
        subprocess.run(['cobc', '--version'], capture_output=True, check=True)
        print("✅ GnuCOBOL détecté")
    except (subprocess.CalledProcessError, FileNotFoundError):
        print("❌ GnuCOBOL non trouvé. Installez-le avec: sudo apt install gnucobol4")
        sys.exit(1)
    
    # Créer le serveur
    with socketserver.TCPServer(("", PORT), COBOLCGIHandler) as httpd:
        print(f"🚀 Serveur web lancé sur http://localhost:{PORT}")
        print(f"🏦 Interface bancaire: http://localhost:{PORT}/WebBanking.cgi")
        print(f"📁 Fichiers statiques: http://localhost:{PORT}/")
        print("🛑 Appuyez sur Ctrl+C pour arrêter")
        
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            print("\n🛑 Arrêt du serveur")
            httpd.shutdown()

if __name__ == "__main__":
    main()
